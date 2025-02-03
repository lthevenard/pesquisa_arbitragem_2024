## Libraries ----

library(tidyverse)
library(lubridate)
library(readxl)

## Functions ----

save_plot <- function(filename, path = "./2024_06_26/plots", height = 6, width = 10, dpi = 300, ...) {
  ggsave(filename=filename, height=height, width=width, path=path, dpi=dpi, ...)
}

count_and_perc <- function(df) {
  df %>% 
    summarise(N = n()) %>%
    mutate(percentual = (N / sum(N)) %>% map_chr(scales::label_percent(accuracy=0.1))) %>%
    ungroup()
}

export_table <- function(df, filename) {
  df %>% 
    write_csv2(paste0("./2024_06_26/dados/", filename))
}

## Data ----

estadual <- read_xlsx("2024_06_26/estadual.xlsx", sheet = "arbitragens", col_types = rep("text", 27)) %>%
  mutate(data_inicio = ifelse(str_detect(data_inicio, "^\\d+$"), 
                              as_date(as.numeric(data_inicio), origin = "1899-12-30") %>% as.character(),
                              data_inicio),
         data_termo = ifelse(str_detect(data_termo, "^\\d+$"), 
                             as_date(as.numeric(data_termo), origin = "1899-12-30") %>% as.character(),
                             data_termo),
         data_fim = ifelse(str_detect(data_fim, "^\\d+$"), 
                           as_date(as.numeric(data_fim), origin = "1899-12-30") %>% as.character(),
                           data_fim)) %>%
  mutate(
    data_inicio = case_when(
      str_detect(data_inicio, "^\\d{4}-\\d{2}-\\d{2}$") ~ ymd(data_inicio) %>% as.character(),
      str_detect(data_inicio, "^\\d{2}-\\d{2}-\\d{4}$") ~ dmy(data_inicio) %>% as.character(),
      TRUE ~ data_inicio
    ),
    data_termo = case_when(
      str_detect(data_termo, "^\\d{4}-\\d{2}-\\d{2}$") ~ ymd(data_termo) %>% as.character(),
      str_detect(data_termo, "^\\d{2}-\\d{2}-\\d{4}$") ~ dmy(data_termo) %>% as.character(),
      TRUE ~ data_termo
    ),
    data_fim = case_when(
      str_detect(data_fim, "^\\d{4}-\\d{2}-\\d{2}$") ~ ymd(data_fim) %>% as.character(),
      str_detect(data_fim, "^\\d{2}-\\d{2}-\\d{4}$") ~ dmy(data_fim) %>% as.character(),
      TRUE ~ data_fim
    ))


federal <- read_xlsx("2024_06_26/federal.xlsx", sheet = "arbitragens", col_types = rep("text", 27)) %>%
  mutate(data_inicio = ifelse(str_detect(data_inicio, "^\\d+$"), 
                              as_date(as.numeric(data_inicio), origin = "1899-12-30") %>% as.character(),
                              data_inicio),
         data_termo = ifelse(str_detect(data_termo, "^\\d+$"), 
                             as_date(as.numeric(data_termo), origin = "1899-12-30") %>% as.character(),
                             data_termo),
         data_fim = ifelse(str_detect(data_fim, "^\\d+$"), 
                           as_date(as.numeric(data_fim), origin = "1899-12-30") %>% as.character(),
                           data_fim)) %>%
  mutate(
    data_inicio = case_when(
      str_detect(data_inicio, "^\\d{4}-\\d{2}-\\d{2}$") ~ ymd(data_inicio) %>% as.character(),
      str_detect(data_inicio, "^\\d{2}-\\d{2}-\\d{4}$") ~ dmy(data_inicio) %>% as.character(),
      TRUE ~ data_inicio
    ),
    data_termo = case_when(
      str_detect(data_termo, "^\\d{4}-\\d{2}-\\d{2}$") ~ ymd(data_termo) %>% as.character(),
      str_detect(data_termo, "^\\d{2}-\\d{2}-\\d{4}$") ~ dmy(data_termo) %>% as.character(),
      TRUE ~ data_termo
    ),
    data_fim = case_when(
      str_detect(data_fim, "^\\d{4}-\\d{2}-\\d{2}$") ~ ymd(data_fim) %>% as.character(),
      str_detect(data_fim, "^\\d{2}-\\d{2}-\\d{4}$") ~ dmy(data_fim) %>% as.character(),
      TRUE ~ data_fim
    ))

df <- bind_rows(federal, estadual)

## Analysis ----

theme_set({{theme_bw() + theme(legend.position = "bottom")}})

pal <- colorspace::choose_palette()
single <- pal(5)[2]

subtitle <- "Pesquisa de arbitragens (2002-2022)"

### Universo analisado, por ano ----

categorias_ano <- c(
  "2002-2006",
  "2007-2012",
  "2013-2017",
  "2018-2022",
  "Data de início\nnão disponível"
)

df_universo_ano <- df %>%
  mutate(ano_inicio = case_when(
    data_inicio == "N/D" ~ categorias_ano[5], 
    str_extract(data_inicio, "\\d{4}") %in% as.character(2002:2006) ~ categorias_ano[1],
    str_extract(data_inicio, "\\d{4}") %in% as.character(2007:2012) ~ categorias_ano[2],
    str_extract(data_inicio, "\\d{4}") %in% as.character(2013:2017) ~ categorias_ano[3],
    str_extract(data_inicio, "\\d{4}") %in% as.character(2018:2022) ~ categorias_ano[4]
  ) %>% factor(ordered = TRUE, levels = categorias_ano))

df_universo_ano %>%
  group_by(ano_inicio, uf_arbitragem) %>%
  count_and_perc() %>% 
  arrange(ano_inicio, uf_arbitragem) %>%
  export_table("1_universo_ano.csv")

df_universo_ano %>%
  ggplot(aes(x = ano_inicio, fill = fct_infreq(uf_arbitragem))) +
  geom_bar() +
  scale_fill_manual(values = pal(4)) +
  scale_x_discrete(drop = FALSE) +
  labs(title = "Arbitragens ao longo do tempo, por Unidade Federativa",
       subtitle = subtitle,
       x = "Ano de início da arbitragem",
       y = "Número de arbitragens identificadas",
       fill = "Unidade Federativa: ")

save_plot("1_universo_ano.png")


### Câmara, UF ----

categorias_camaras <- c("CCI", "Outras Câmaras", "Ad Hoc", "N/D")

df_camaras_uf <- df %>%
  mutate(cam_class = case_when(
    camara %in% categorias_camaras[c(1, 3:4)] ~ camara,
    TRUE ~ "Outras Câmaras"
  ))

df_camaras_uf %>%
  group_by(camara) %>%
  count_and_perc() %>% 
  arrange(camara) %>% 
  export_table("2_camaras.csv")

df_camaras_uf %>%
  mutate(cam_class = factor(
    cam_class, ordered = TRUE, levels = categorias_camaras
  )) %>%
  ggplot(aes(x = cam_class, fill = fct_infreq(uf_arbitragem))) +
  geom_bar() +
  scale_fill_manual(values = pal(4)) +
  labs(title = "Câmaras das arbitragens, por Unidade Federativa",
       subtitle = subtitle,
       y = "Número de arbitragens identificadas",
       x = "Câmaras", fill = "Unidade Federativa: ")

save_plot("2_camaras_uf.png")

df_camaras_uf %>%
  mutate(cam_class = factor(
    cam_class, ordered = TRUE, levels = categorias_camaras
  )) %>%
  ggplot(aes(fill = cam_class, x = fct_infreq(uf_arbitragem))) +
  geom_bar() +
  scale_fill_manual(values = c(pal(5)[3:2], "darkgray", "lightgray")) +
  labs(title = "Câmaras das arbitragens, por Unidade Federativa",
       subtitle = subtitle,
       y = "Número de arbitragens identificadas",
       x = "Unidade Federativa", fill = "Câmaras: ")

save_plot("2_uf_camaras.png")


### Objeto ----

df_objetos <- df %>% 
  mutate(objeto = str_split(objeto, "\\s*;\\s*")) %>%
  unnest(cols = objeto) %>%
  mutate(objeto = str_to_sentence(objeto) %>% str_replace("(?<=/)d", "D")) %>% 
  count(objeto) %>%
  mutate(perc = n / 54,
         percentual = map_chr(perc, scales::label_percent(accuracy = 0.1)))

df_objetos %>%
  export_table("3_objetos.csv")

df_objetos %>%
  mutate(fill_class = objeto == "N/D") %>%
  ggplot(aes(y = fct_reorder(objeto, n) %>%
               fct_relevel("N/D"),
             x = perc,
             fill = fill_class)) +
  geom_col() +
  scale_fill_manual(values = c(single, "darkgray")) +
  scale_x_continuous(labels = scales::label_percent()) +
  theme(legend.position = "none") +
  labs(title = "Objetos das arbitragens",
       subtitle = subtitle,
       y = "Objeto", x = "Percentual, das arbitragens (N=54), no qual o objeto está presente")

save_plot("3_objetos.png")

### Resultado Final ----

df_resultado_final <- df %>%
  mutate(resultado_final = ifelse(
    str_detect(resultado_final,";"), "Improcedente", resultado_final
  )) %>% 
  filter(resultado_final != "N/A") %>% 
  group_by(resultado_final) %>%
  count_and_perc()

df_resultado_final %>% 
  arrange(fct_infreq(resultado_final) %>% 
            fct_relevel("N/D", after = Inf)) %>%
  export_table("4_resultado_final.csv")

df_resultado_final %>%
  mutate(lbl = paste0(N, " (", percentual, ")"),
         fill_case = resultado_final == "N/D") %>%
  ggplot(aes(y = fct_reorder(resultado_final, N) %>% 
               fct_rev() %>% fct_relevel("N/D"),
             x = N,
             fill = fill_case,
             color = fill_case,
             label = lbl)) +
  geom_col() +
  geom_label(nudge_x = 0.5, fill = "white") +
  scale_fill_manual(values = c(single, "darkgray")) +
  scale_color_manual(values = c(single, "darkgray")) +
  scale_x_continuous(breaks = seq(0, 8, 2), limits = c(0, 9)) +
  theme(legend.position = "none") +
  labs(title = "Resultado final das arbitragens encerradas",
       subtitle = subtitle,
       x = "Número de arbitragens",
       y = "Resultado")

save_plot("4_resultado_final.png")

### Setor ----

df %>%
  filter(setor != "N/A") %>% 
  group_by(setor) %>% 
  count_and_perc() %>%
  export_table("5_setores.csv")
  
df %>%
  filter(setor != "N/A") %>%   
  mutate(setor = fct_lump_min(setor, 2, other_level = "Outros Setores")) %>% 
  ggplot(aes(y = fct_infreq(setor) %>% fct_rev())) +
  geom_bar(fill = single) +
  labs(title = "Setores das arbitragens",
       subtitle = subtitle,
       x = "Número de arbitragens",
       y = "Setor")

save_plot("5_setores.png")

### Reconvenção ----

df %>%
  group_by(houve_reconvencao) %>% 
  count_and_perc() %>% 
  export_table("6_reconvencao.csv")

df %>% 
  mutate(houve_reconvencao = factor(
    houve_reconvencao,
    ordered = TRUE,
    levels = c("Sim", "Não", "N/A", "N/D")
  )) %>% 
  group_by(houve_reconvencao) %>%
  summarise(n = n()) %>%
  mutate(perc = n / sum(n),
         percentual = map_chr(perc, scales::label_percent(accuracy = 0.1)),
         lbl = paste0(n, " (", percentual, ")")) %>%
  mutate(fill_case = houve_reconvencao %in% c("Sim", "Não")) %>% 
  ggplot(aes(x = houve_reconvencao, y = n,
             color = fill_case, fill = fill_case, label = lbl)) +
  geom_col() +
  geom_label(nudge_y = 1.4, fill = "white") +
  scale_fill_manual(values = c("darkgray", single)) +
  scale_color_manual(values = c("darkgray", single)) +
  labs(title = "Houve reconvenção?",
       subtitle = subtitle,
       y = "Número de arbitragens",
       x = "Houve reconvenção?") +
  theme(legend.position = "none")

save_plot("6_reconvencao.png")


### Sentença Parcial ----

df %>%
  group_by(teve_sentenca_parcial) %>% 
  count_and_perc() %>% 
  export_table("7_sentenca_parcial.csv")

df %>% 
  mutate(teve_sentenca_parcial = factor(
    teve_sentenca_parcial,
    ordered = TRUE,
    levels = c("Sim", "Não", "N/A", "N/D")
  )) %>% 
  group_by(teve_sentenca_parcial) %>%
  summarise(n = n()) %>%
  mutate(perc = n / sum(n),
         percentual = map_chr(perc, scales::label_percent(accuracy = 0.1)),
         lbl = paste0(n, " (", percentual, ")")) %>%
  mutate(fill_case = teve_sentenca_parcial %in% c("Sim", "Não")) %>% 
  ggplot(aes(x = teve_sentenca_parcial, y = n,
             color = fill_case, fill = fill_case, label = lbl)) +
  geom_col() +
  geom_label(nudge_y = 1, fill = "white") +
  scale_fill_manual(values = c("darkgray", single)) +
  scale_color_manual(values = c("darkgray", single)) +
  labs(title = "Houve sentença parcial?",
       subtitle = subtitle,
       y = "Número de arbitragens",
       x = "Houve sentença parcial?") +
  theme(legend.position = "none")

save_plot("7_sentenca_parcial.png")

### Resultado da sentença parcial ----

df_resultado_parcial <- df %>%
  filter(!str_detect(resultado_sentenca_parcial,";")) %>% 
  group_by(resultado_sentenca_parcial) %>%
  count_and_perc()

df_resultado_parcial %>% 
  arrange(fct_infreq(resultado_sentenca_parcial) %>% 
            fct_relevel("N/D", after = Inf)) %>%
  export_table("8_resultado_parcial.csv")

df_resultado_parcial %>%
  filter(resultado_sentenca_parcial != "N/A") %>% 
  mutate(lbl = paste0(N, " (", percentual, ")"),
         fill_case = resultado_sentenca_parcial == "N/D") %>%
  ggplot(aes(y = fct_reorder(resultado_sentenca_parcial, N) %>% 
               fct_rev() %>% fct_relevel("N/D"),
             x = N,
             fill = fill_case,
             color = fill_case,
             label = lbl)) +
  geom_col() +
  geom_label(nudge_x = 0.8, fill = "white") +
  scale_fill_manual(values = c(single, "darkgray")) +
  scale_color_manual(values = c(single, "darkgray")) +
  scale_x_continuous(breaks = seq(0, 12, 2), limits = c(0, 12)) +
  theme(legend.position = "none") +
  labs(title = "Resultado da sentença parcial",
       subtitle = subtitle,
       x = "Número de arbitragens",
       y = "Resultado da sentença parcial")

save_plot("8_resultado_sentenca_parcial.png")

### Liminar Pré-Arbitral ----

df %>%
  group_by(teve_liminar_pre_arbitral) %>% 
  count_and_perc() %>% 
  export_table("9_liminar_pre_arbitral.csv")

df %>% 
  mutate(teve_liminar_pre_arbitral = factor(
    teve_liminar_pre_arbitral,
    ordered = TRUE,
    levels = c("Sim", "Não", "N/A", "N/D")
  )) %>% 
  group_by(teve_liminar_pre_arbitral) %>%
  summarise(n = n()) %>%
  mutate(perc = n / sum(n),
         percentual = map_chr(perc, scales::label_percent(accuracy = 0.1)),
         lbl = paste0(n, " (", percentual, ")")) %>%
  mutate(fill_case = teve_liminar_pre_arbitral %in% c("Sim", "Não")) %>% 
  ggplot(aes(x = teve_liminar_pre_arbitral, y = n,
             color = fill_case, fill = fill_case, label = lbl)) +
  geom_col() +
  geom_label(nudge_y = 1, fill = "white") +
  scale_fill_manual(values = c("darkgray", single)) +
  scale_color_manual(values = c("darkgray", single)) +
  labs(title = "Houve liminar pré-arbitral?",
       subtitle = subtitle,
       y = "Número de arbitragens",
       x = "Houve liminar pré-arbitral?") +
  theme(legend.position = "none")

save_plot("9_liminar_pre_arbitral.png")


### Intevenção TCU ----

df %>%
  group_by(houve_intervencao_tcu) %>% 
  count_and_perc() %>% 
  export_table("10_intervencao_tcu.csv")

df %>% 
  mutate(houve_intervencao_tcu = factor(
    houve_intervencao_tcu,
    ordered = TRUE,
    levels = c("Sim", "Não", "N/A", "N/D")
  )) %>% 
  group_by(houve_intervencao_tcu) %>%
  summarise(n = n()) %>%
  mutate(perc = n / sum(n),
         percentual = map_chr(perc, scales::label_percent(accuracy = 0.1)),
         lbl = paste0(n, " (", percentual, ")")) %>%
  mutate(fill_case = houve_intervencao_tcu %in% c("Sim", "Não")) %>% 
  ggplot(aes(x = houve_intervencao_tcu, y = n,
             color = fill_case, fill = fill_case, label = lbl)) +
  geom_col() +
  geom_label(nudge_y = 1.4, fill = "white") +
  scale_fill_manual(values = c("darkgray", single)) +
  scale_color_manual(values = c("darkgray", single)) +
  labs(title = "Houve intervenção do TCU?",
       subtitle = subtitle,
       y = "Número de arbitragens",
       x = "Houve intervenção do TCU?") +
  theme(legend.position = "none")

save_plot("10_intervencao_tcu.png")


### Intevenção de terceiros ----

df %>%
  group_by(houve_intervencao_terceiros) %>% 
  count_and_perc() %>% 
  export_table("11_intervencao_terceiros.csv")

df %>% 
  mutate(houve_intervencao_terceiros = factor(
    houve_intervencao_terceiros,
    ordered = TRUE,
    levels = c("Sim", "Não", "N/A", "N/D")
  )) %>% 
  group_by(houve_intervencao_terceiros) %>%
  summarise(n = n()) %>%
  mutate(perc = n / sum(n),
         percentual = map_chr(perc, scales::label_percent(accuracy = 0.1)),
         lbl = paste0(n, " (", percentual, ")")) %>%
  mutate(fill_case = houve_intervencao_terceiros %in% c("Sim", "Não")) %>% 
  ggplot(aes(x = houve_intervencao_terceiros, y = n,
             color = fill_case, fill = fill_case, label = lbl)) +
  geom_col() +
  geom_label(nudge_y = 1.4, fill = "white") +
  scale_fill_manual(values = c("darkgray", single)) +
  scale_color_manual(values = c("darkgray", single)) +
  labs(title = "Houve intervenção de terceiros?",
       subtitle = subtitle,
       y = "Número de arbitragens",
       x = "Houve intervenção de terceiros?") +
  theme(legend.position = "none")

save_plot("11_intervencao_terceiros.png")


### Precatórios ----

df %>%
  group_by(caiu_em_precatorio) %>% 
  count_and_perc() %>% 
  export_table("12_precatorios.csv")

df %>% 
  mutate(caiu_em_precatorio = factor(
    caiu_em_precatorio,
    ordered = TRUE,
    levels = c("Sim", "Não", "N/A", "N/D")
  )) %>% 
  group_by(caiu_em_precatorio) %>%
  summarise(n = n()) %>%
  mutate(perc = n / sum(n),
         percentual = map_chr(perc, scales::label_percent(accuracy = 0.1)),
         lbl = paste0(n, " (", percentual, ")")) %>%
  mutate(fill_case = caiu_em_precatorio %in% c("Sim", "Não")) %>% 
  ggplot(aes(x = caiu_em_precatorio, y = n,
             color = fill_case, fill = fill_case, label = lbl)) +
  geom_col() +
  geom_label(nudge_y = 1.4, fill = "white") +
  scale_fill_manual(values = c("darkgray", single)) +
  scale_color_manual(values = c("darkgray", single)) +
  labs(title = "Caiu em precatório?",
       subtitle = subtitle,
       y = "Número de arbitragens",
       x = "Caiu em precatório?") +
  theme(legend.position = "none")

save_plot("12_precatorios.png")


### Tempo termo ----

df_tempo_ate_termo <- df_universo_ano %>%
  filter(!(data_inicio %in% c("N/A", "N/D")) & !(data_termo %in% c("N/A", "N/D"))) %>% 
  mutate(data_inicio = ymd(data_inicio),
         data_termo = ymd(data_termo),
         dias_ate_termo = as.numeric(data_termo) - as.numeric(data_inicio))

levels(df_tempo_ate_termo$ano_inicio)[5] <- NA

df_tempo_ate_termo %>%
  ggplot(aes(x = uf_arbitragem, y = dias_ate_termo)) +
  geom_boxplot(fill = single) +
  scale_y_continuous(limits = c(0, 750)) +
  labs(title = "Tempo até o termo arbitral, por Unidade Federativa",
       subtitle = subtitle,
       x = "Unidade Federativa",
       y = "Tempo, em dias, até o termo arbitral")

save_plot("13_tempo_termo_uf.png")

df_tempo_ate_termo %>%
  ggplot(aes(x = ano_inicio, y = dias_ate_termo)) +
  geom_boxplot(fill = single) +
  scale_y_continuous(limits = c(0, 750)) +
  scale_x_discrete(drop = FALSE) +
  labs(title = "Tempo até o termo arbitral, por ano de início da arbitragem",
       subtitle = subtitle,
       x = "Ano de início da arbitragem",
       y = "Tempo, em dias, até o termo arbitral")

save_plot("13_tempo_termo_ano.png")



### Tempo fim ----

df_tempo_ate_fim <- df_universo_ano %>%
  filter(!(data_inicio %in% c("N/A", "N/D")) & !(data_fim %in% c("N/A", "N/D"))) %>% 
  mutate(data_inicio = ymd(data_inicio),
         data_termo = ymd(data_fim),
         dias_ate_fim = as.numeric(data_termo) - as.numeric(data_inicio))

count_uf_tempo_fim <- df_tempo_ate_fim %>% 
  count(uf_arbitragem) %>%
  mutate(uf_n = paste0(uf_arbitragem, "\n(N = ", n, ")")) %>%
  select(!n)

levels(df_tempo_ate_fim$ano_inicio)[5] <- NA

df_tempo_ate_fim %>%
  left_join(count_uf_tempo_fim, by = "uf_arbitragem") %>% 
  ggplot(aes(x = uf_n, y = dias_ate_fim)) +
  geom_boxplot(fill = single) +
  scale_y_continuous(limits = c(0, 2100)) +
  labs(title = "Tempo até o fim da arbitragem, por Unidade Federativa",
       subtitle = subtitle,
       x = "Unidade Federativa",
       y = "Tempo, em dias, até o fim da arbitragem")

save_plot("14_tempo_fim_uf.png")

df_tempo_ate_fim %>%
  ggplot(aes(x = ano_inicio, y = dias_ate_fim)) +
  geom_boxplot(fill = single) +
  scale_x_discrete(drop = FALSE) +
  labs(title = "Tempo até o fim da arbitragem, por ano de início da arbitragem",
       subtitle = subtitle,
       x = "Ano de início da arbitragem",
       y = "Tempo, em dias, até o fim da arbitragem")

save_plot("14_tempo_fim_ano.png")





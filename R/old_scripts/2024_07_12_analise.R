## Libraries ----

library(tidyverse)
library(lubridate)
library(readxl)

## Functions ----

save_plot <- function(filename, path = "./2024_07_12/plots", height = 6, width = 10, dpi = 300, ...) {
  ggsave(filename=filename, height=height, width=width, path=path, dpi=dpi, ...)
}

count_and_perc <- function(df, add_label = FALSE, accur = 0.1) {
  output <- df %>% 
    summarise(n = n()) %>%
    mutate(percentual = (n / sum(n)) %>% map_chr(scales::label_percent(accuracy=accur))) %>%
    ungroup()
  
  if (add_label) {
    output <- output %>%
      mutate(lbl = paste0(n, " (", percentual, ")"))
  }
  return(output)
}

export_table <- function(df, filename, folder = "./2024_07_12/dados/") {
  df %>% 
    write_csv2(paste0(folder, filename))
}

calc_date_dist <- function(date_begin, date_end) {
  if (date_end %in% c("N/A", "N/D") | date_begin %in% c("N/A", "N/D")) {
    return(NA)
  } else {
    return(
      as.numeric(ymd(date_end)) - as.numeric(ymd(date_begin))
    )
  }
}

## Data ----

df <- read_xlsx("2024_07_12/dados.xlsx", sheet = "arbitragens", col_types = rep("text", 31)) %>% 
  janitor::clean_names() %>% 
  mutate(data_inicio = ifelse(str_detect(data_inicio, "^\\d+$"), 
                              as_date(as.numeric(data_inicio), origin = "1899-12-30") %>% as.character(),
                              data_inicio),
         data_termo = ifelse(str_detect(data_termo, "^\\d+$"), 
                             as_date(as.numeric(data_termo), origin = "1899-12-30") %>% as.character(),
                             data_termo),
         data_fim = ifelse(str_detect(data_fim, "^\\d+$"), 
                           as_date(as.numeric(data_fim), origin = "1899-12-30") %>% as.character(),
                           data_fim),
         inicio_contrato = ifelse(str_detect(inicio_contrato, "^\\d+$"), 
                           as_date(as.numeric(inicio_contrato), origin = "1899-12-30") %>% as.character(),
                           inicio_contrato),
         data_problema = ifelse(str_detect(data_problema, "^\\d+$"), 
                           as_date(as.numeric(data_problema), origin = "1899-12-30") %>% as.character(),
                           data_problema)) %>%
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
    ),
    inicio_contrato = case_when(
      str_detect(inicio_contrato, "^\\d{4}-\\d{2}-\\d{2}$") ~ ymd(inicio_contrato) %>% as.character(),
      str_detect(inicio_contrato, "^\\d{2}-\\d{2}-\\d{4}$") ~ dmy(inicio_contrato) %>% as.character(),
      TRUE ~ inicio_contrato
    ),
    data_problema = case_when(
      str_detect(data_problema, "^\\d{4}-\\d{2}-\\d{2}$") ~ ymd(data_problema) %>% as.character(),
      str_detect(data_problema, "^\\d{2}-\\d{2}-\\d{4}$") ~ dmy(data_problema) %>% as.character(),
      TRUE ~ data_problema
    ))

categorias_ano <- c(
  "2002-2006",
  "2007-2012",
  "2013-2017",
  "2018-2023",
  "N/D"
)

categorias_camaras <- c(
  "CCI",
  "Outras Câmaras",
  "Ad Hoc",
  "N/D"
)

categorias_sim_nao <- c(
  "Sim",
  "Não",
  "N/A",
  "N/D"
)

df <- df %>%
  mutate(
    uf_arbitragem = fct_infreq(uf_arbitragem),
    ano_inicio = ifelse(data_inicio == "N/D", "N/D", str_extract(data_inicio, "\\d{4}")) %>%
      factor(ordered = T, levels = c(as.character(2002:2023), "N/D")),
    periodo_inicio = case_when(
      data_inicio == "N/D" ~ data_inicio, 
      str_extract(data_inicio, "\\d{4}") %in% as.character(2002:2006) ~ categorias_ano[1],
      str_extract(data_inicio, "\\d{4}") %in% as.character(2007:2012) ~ categorias_ano[2],
      str_extract(data_inicio, "\\d{4}") %in% as.character(2013:2017) ~ categorias_ano[3],
      str_extract(data_inicio, "\\d{4}") %in% as.character(2018:2023) ~ categorias_ano[4]
    ) %>% factor(ordered = T, levels = categorias_ano),
    cam_class = case_when(
      camara %in% categorias_camaras[c(1, 3:4)] ~ camara,
      TRUE ~ "Outras Câmaras"
    ) %>% factor(ordered = T, levels = categorias_camaras),
    resultado_final = ifelse(
      str_detect(resultado_final,";"), "Improcedente", resultado_final
    ) %>% fct_infreq() %>% fct_relevel("N/A", after = Inf) %>% fct_relevel("N/D", after = Inf),
    houve_reconvencao = factor(
      houve_reconvencao,
      ordered = TRUE,
      levels = categorias_sim_nao
    ),
    teve_sentenca_parcial = factor(
      teve_sentenca_parcial,
      ordered = TRUE,
      levels = categorias_sim_nao
    ),
    teve_liminar_pre_arbitral = factor(
      teve_liminar_pre_arbitral,
      ordered = TRUE,
      levels = categorias_sim_nao
    ),
    houve_intervencao_tcu = factor(
      houve_intervencao_tcu,
      ordered = TRUE,
      levels = categorias_sim_nao
    ),
    houve_intervencao_terceiros = factor(
      houve_intervencao_terceiros,
      ordered = TRUE,
      levels = categorias_sim_nao
    ),
    caiu_em_precatorio = factor(
      caiu_em_precatorio,
      ordered = TRUE,
      levels = c("Sim", "Não", "N/A", "N/D")
    )
  )

## Analysis ----

theme_set({{theme_bw() + theme(legend.position = "bottom")}})

pal <- colorspace::choose_palette()
single <- pal(5)[2]

subtitle <- "Pesquisa de arbitragens (2002-2022)"

### Universo analisado, por ano ----

df %>%
  group_by(periodo_inicio, uf_arbitragem) %>%
  count_and_perc() %>% 
  arrange(periodo_inicio, uf_arbitragem) %>%
  export_table("1_universo_periodo.csv")

df %>%
  ggplot(aes(x = periodo_inicio, fill = uf_arbitragem)) +
  geom_bar(position = "dodge") +
  scale_fill_manual(values = pal(4)) +
  scale_x_discrete(drop = FALSE) +
  labs(title = "Arbitragens ao longo do tempo, por Unidade Federativa",
       subtitle = subtitle,
       x = "Período de início da arbitragem",
       y = "Número de arbitragens identificadas",
       fill = "Unidade Federativa: ")

save_plot("1_universo_periodo_dodge.png")

df %>%
  ggplot(aes(x = periodo_inicio, fill = uf_arbitragem)) +
  geom_bar(position = "dodge") +
  scale_fill_manual(values = pal(4)) +
  scale_x_discrete(drop = FALSE) +
  labs(title = "Arbitragens ao longo do tempo, por Unidade Federativa",
       subtitle = subtitle,
       x = "\nPeríodo de início da arbitragem",
       y = "Número de arbitragens identificadas",
       fill = "Unidade Federativa: ") +
  facet_wrap(~uf_arbitragem) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        legend.position = "none")

save_plot("1_universo_periodo_facet.png")

df %>%
  ggplot(aes(x = ano_inicio, fill = uf_arbitragem)) +
  geom_bar() +
  scale_fill_manual(values = pal(4)) +
  scale_x_discrete(drop = FALSE) +
  labs(title = "Arbitragens ao longo do tempo, por Unidade Federativa",
       subtitle = subtitle,
       x = "Ano de início da arbitragem",
       y = "Número de arbitragens identificadas",
       fill = "Unidade Federativa: ") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

save_plot("1_universo_ano_stack.png")

df %>%
  mutate(ano_inicio = ifelse(data_inicio == "N/D", "N/D", str_extract(data_inicio, "\\d{4}")) %>%
           factor(ordered = T, levels = c(as.character(2002:2023), "", "N/D"))) %>% 
  ggplot(aes(x = ano_inicio, fill = uf_arbitragem)) +
  geom_bar() +
  geom_vline(xintercept = "", size = 0.25) +
  scale_fill_manual(values = pal(4)) +
  scale_x_discrete(drop = FALSE, expand = expansion(mult = c(0.045, 0.045))) +
  labs(title = "Arbitragens ao longo do tempo, por Unidade Federativa",
       subtitle = subtitle,
       x = "Ano de início da arbitragem",
       y = "Número de arbitragens identificadas",
       fill = "Unidade Federativa: ") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        legend.position = "none",
        axis.ticks.x = element_line(colour =c(rep("black", 22), "transparent", "black")))

save_plot("1_universo_ano_stack.png")

### Câmara, UF ----

df %>%
  group_by(camara) %>%
  count_and_perc() %>% 
  arrange(camara) %>% 
  export_table("2_camaras.csv")

df %>%
  ggplot(aes(x = cam_class, fill = uf_arbitragem)) +
  geom_bar() +
  scale_fill_manual(values = pal(4)) +
  labs(title = "Câmaras das arbitragens, por Unidade Federativa",
       subtitle = subtitle,
       y = "Número de arbitragens identificadas",
       x = "Câmaras", fill = "Unidade Federativa: ")

save_plot("2_camaras_uf_stack.png")

df %>%
  ggplot(aes(x = cam_class, fill = uf_arbitragem)) +
  geom_bar(position = "dodge") +
  scale_fill_manual(values = pal(4)) +
  labs(title = "Câmaras das arbitragens, por Unidade Federativa",
       subtitle = subtitle,
       y = "Número de arbitragens identificadas",
       x = "Câmaras", fill = "Unidade Federativa: ")

save_plot("2_camaras_uf_dodge.png")

df %>%
  ggplot(aes(fill = cam_class, x = uf_arbitragem)) +
  geom_bar() +
  scale_fill_manual(values = c(pal(5)[3:2], "darkgray", "lightgray")) +
  labs(title = "Câmaras das arbitragens, por Unidade Federativa",
       subtitle = subtitle,
       y = "Número de arbitragens identificadas",
       x = "Unidade Federativa", fill = "Câmaras: ")

save_plot("2_uf_camaras_stack.png")

df %>%
  ggplot(aes(fill = cam_class, x = uf_arbitragem)) +
  geom_bar(position = "dodge") +
  scale_fill_manual(values = c(pal(5)[3:2], "darkgray", "lightgray")) +
  labs(title = "Câmaras das arbitragens, por Unidade Federativa",
       subtitle = subtitle,
       y = "Número de arbitragens identificadas",
       x = "Unidade Federativa", fill = "Câmaras: ")

save_plot("2_uf_camaras_dodge.png")

df %>%
  ggplot(aes(x = cam_class, fill = cam_class)) +
  geom_bar(position = "dodge") +
  scale_fill_manual(values = c(pal(5)[3:2], "darkgray", "lightgray")) +
  labs(title = "Câmaras das arbitragens, por Unidade Federativa",
       subtitle = subtitle,
       y = "Número de arbitragens identificadas",
       x = "Unidade Federativa", fill = "Câmaras: ") +
  facet_wrap(~uf_arbitragem) +
  theme(legend.position = "none")

save_plot("2_uf_camaras_facet.png")

### Objeto ----

n_arbitragens <- nrow(df)

df_objetos <- df %>% 
  mutate(objeto = str_split(objeto, "\\s*;\\s*")) %>%
  unnest(cols = objeto) %>%
  mutate(objeto = str_to_sentence(objeto) %>% str_replace("(?<=/)d", "D")) %>% 
  count(objeto) %>%
  mutate(perc = n / n_arbitragens,
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
  filter(resultado_final != "N/A") %>% 
  group_by(resultado_final) %>%
  count_and_perc()

df_resultado_final %>% 
  arrange(resultado_final) %>% 
  export_table("4_resultado_final.csv")

df_resultado_final %>%
  mutate(lbl = paste0(n, " (", percentual, ")"),
         fill_case = resultado_final == "N/D") %>%
  ggplot(aes(y = fct_reorder(resultado_final, n) %>% 
               fct_rev() %>% fct_relevel("N/D"),
             x = n,
             fill = fill_case,
             color = fill_case,
             label = lbl)) +
  geom_col() +
  geom_label(nudge_x = 0.6, fill = "white") +
  scale_fill_manual(values = c(single, "darkgray")) +
  scale_color_manual(values = c(single, "darkgray")) +
  scale_x_continuous(breaks = seq(0, 10, 2), limits = c(0, 10.1)) +
  theme(legend.position = "none") +
  labs(title = "Resultado final das arbitragens encerradas",
       subtitle = subtitle,
       x = "Número de arbitragens",
       y = "Resultado")

save_plot("4_resultado_final.png")

df %>%
  filter(resultado_final != "N/A") %>% 
  ggplot(aes(y = resultado_final %>% 
               fct_rev() %>% 
               fct_relevel("N/D"),
             fill = uf_arbitragem)) +
  geom_bar() +
  scale_fill_manual(values = pal(4)) +
  scale_x_continuous(breaks = seq(0, 10, 2), limits = c(0, 10.1)) +
  labs(title = "Resultado final das arbitragens encerradas",
       subtitle = subtitle,
       x = "Número de arbitragens",
       y = "Resultado",
       fill = "Unidade Federativa: ")

save_plot("4_resultado_final_stack.png")

### Setor ----

df_setor <- df %>%
  filter(setor != "N/A") %>%   
  mutate(setor = fct_lump_min(setor, 2, other_level = "Outros Setores") %>% 
           fct_infreq())

df %>%
  filter(setor != "N/A") %>% 
  group_by(setor) %>% 
  count_and_perc() %>%
  export_table("5_setores.csv")

df_setor %>% 
  ggplot(aes(y = fct_infreq(setor) %>% fct_rev())) +
  geom_bar(fill = single) +
  labs(title = "Setores das arbitragens",
       subtitle = subtitle,
       x = "Número de arbitragens",
       y = "Setor")

save_plot("5_setores.png")

df_setor %>%
  ggplot(aes(y = setor %>% fct_rev(), fill = uf_arbitragem)) +
  geom_bar() +
  scale_fill_manual(values = pal(4)) +
  labs(title = "Setores das arbitragens",
       subtitle = subtitle,
       x = "Número de arbitragens",
       y = "Setor",
       fill = "Unidade Federativa: ")

save_plot("5_setores_stack.png")

### Reconvenção ----

df %>%
  group_by(houve_reconvencao) %>%
  count_and_perc() %>%
  export_table("6_reconvencao.csv")

df %>%
  group_by(houve_reconvencao) %>%
  count_and_perc(add_label = TRUE) %>%
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

df %>%
  ggplot(aes(x = houve_reconvencao, fill = uf_arbitragem)) +
  geom_bar() +
  scale_fill_manual(values = pal(4)) +
  labs(title = "Houve reconvenção?",
       subtitle = subtitle,
       y = "Número de arbitragens",
       x = "Houve reconvenção?",
       fill = "Unidade Federativa: ")

save_plot("6_reconvencao_stack.png")

df %>%
  group_by(uf_arbitragem, houve_reconvencao) %>%
  count_and_perc(add_label = TRUE) %>%
  ggplot(aes(x = houve_reconvencao, y = n, fill = uf_arbitragem, 
             color = uf_arbitragem, label = lbl)) +
  geom_col(color = "transparent", position = "dodge") +
  geom_label(aes(y = n+1), fill = "white",
             position = position_dodge(width = .9)) +
  scale_fill_manual(values = pal(4)) +
  scale_color_manual(values = pal(4)) +
  guides(colour = "none") +
  labs(title = "Houve reconvenção?",
       subtitle = subtitle,
       y = "Número de arbitragens",
       x = "Houve reconvenção?",
       fill = "Unidade Federativa: ")

save_plot("6_reconvencao_dodge.png")

df %>%
  group_by(uf_arbitragem, houve_reconvencao) %>%
  count_and_perc(add_label = TRUE) %>%
  ggplot(aes(x = houve_reconvencao, y = n, fill = uf_arbitragem, 
             color = uf_arbitragem, label = lbl)) +
  geom_col() +
  geom_label(aes(y = n+1), fill = "white") +
  scale_fill_manual(values = pal(4)) +
  scale_color_manual(values = pal(4)) +
  labs(title = "Houve reconvenção?",
       subtitle = subtitle,
       y = "Número de arbitragens",
       x = "Houve reconvenção?") +
  theme(legend.position = "none") +
  facet_wrap(~uf_arbitragem)

save_plot("6_reconvencao_facet.png")

### Sentença Parcial ----

df %>%
  group_by(teve_sentenca_parcial) %>%
  count_and_perc() %>%
  export_table("7_sentenca_parcial.csv")

df %>%
  group_by(teve_sentenca_parcial) %>%
  count_and_perc(add_label = TRUE) %>%
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

df %>%
  ggplot(aes(x = teve_sentenca_parcial, fill = uf_arbitragem)) +
  geom_bar() +
  scale_fill_manual(values = pal(4)) +
  labs(title = "Houve sentença parcial?",
       subtitle = subtitle,
       y = "Número de arbitragens",
       x = "Houve sentença parcial?",
       fill = "Unidade Federativa: ")

save_plot("7_sentenca_parcial_stack.png")

df %>%
  group_by(uf_arbitragem, teve_sentenca_parcial) %>%
  count_and_perc(add_label = TRUE) %>%
  ggplot(aes(x = teve_sentenca_parcial, y = n, fill = uf_arbitragem, 
             color = uf_arbitragem, label = lbl)) +
  geom_col(color = "transparent", position = "dodge") +
  geom_label(aes(y = n+0.6), fill = "white", size = 3,
             position = position_dodge(width = .9)) +
  scale_fill_manual(values = pal(4)) +
  scale_color_manual(values = pal(4)) +
  guides(colour = "none") +
  labs(title = "Houve sentença parcial?",
       subtitle = subtitle,
       y = "Número de arbitragens",
       x = "Houve sentença parcial?",
       fill = "Unidade Federativa: ")

save_plot("7_sentenca_parcial_dodge.png")

df %>%
  group_by(uf_arbitragem, teve_sentenca_parcial) %>%
  count_and_perc(add_label = TRUE) %>%
  ggplot(aes(x = teve_sentenca_parcial, y = n, fill = uf_arbitragem, 
             color = uf_arbitragem, label = lbl)) +
  geom_col() +
  geom_label(aes(y = n+0.6), fill = "white") +
  scale_fill_manual(values = pal(4)) +
  scale_color_manual(values = pal(4)) +
  labs(title = "Houve sentença parcial?",
       subtitle = subtitle,
       y = "Número de arbitragens",
       x = "Houve sentença parcial?",
       fill = "Unidade Federativa: ") +
  theme(legend.position = "none") +
  facet_wrap(~uf_arbitragem)

save_plot("7_sentenca_parcial_facet.png")

### Resultado da sentença parcial ----

df_resultado_parcial <- df %>%
  group_by(resultado_sentenca_parcial) %>%
  count_and_perc()

df_resultado_parcial %>%
  arrange(fct_infreq(resultado_sentenca_parcial) %>%
            fct_relevel("N/D", after = Inf)) %>%
  export_table("8_resultado_parcial.csv")

df_resultado_parcial %>%
  filter(resultado_sentenca_parcial != "N/A") %>%
  mutate(lbl = paste0(n, " (", percentual, ")"),
         fill_case = resultado_sentenca_parcial == "N/D") %>%
  ggplot(aes(y = fct_reorder(resultado_sentenca_parcial, n) %>%
               fct_rev() %>% fct_relevel("N/D"),
             x = n,
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

df %>%
  filter(resultado_sentenca_parcial != "N/A") %>%
  mutate(resultado_sentenca_parcial = fct_infreq(resultado_sentenca_parcial) %>%
           fct_relevel("N/D", after = Inf) %>%
           fct_rev()) %>% 
  group_by(uf_arbitragem, resultado_sentenca_parcial) %>% 
  count_and_perc() %>% 
  ggplot(aes(y = resultado_sentenca_parcial, x = n, fill = uf_arbitragem)) +
  geom_col() +
  scale_fill_manual(values = pal(4)) +
  labs(title = "Resultado da sentença parcial",
       subtitle = subtitle,
       x = "Número de arbitragens",
       y = "Resultado da sentença parcial",
       fill = "Unidade Federativa: ")

save_plot("8_resultado_sentenca_parcial_stack.png")

df %>%
  filter(resultado_sentenca_parcial != "N/A") %>%
  mutate(resultado_sentenca_parcial = fct_infreq(resultado_sentenca_parcial) %>%
           fct_relevel("N/D", after = Inf) %>%
           fct_rev()) %>% 
  group_by(uf_arbitragem, resultado_sentenca_parcial) %>% 
  count_and_perc(add_label = T) %>% 
  ggplot(aes(y = resultado_sentenca_parcial, x = n, label = lbl,
             fill = uf_arbitragem, color = uf_arbitragem)) +
  geom_col(position = "dodge", color = "transparent") +
  geom_label(aes(x = n+0.5), fill = "white", size = 3,
             position = position_dodge(width = .9)) +
  scale_fill_manual(values = pal(4)) +
  scale_color_manual(values = pal(4)) +
  guides(colour = "none") +
  scale_x_continuous(breaks = seq(0, 10, by = 2)) +
  labs(title = "Resultado da sentença parcial",
       subtitle = subtitle,
       x = "Número de arbitragens",
       y = "Resultado da sentença parcial",
       fill = "Unidade Federativa: ")

save_plot("8_resultado_sentenca_parcial_dodge.png")

df %>%
  filter(resultado_sentenca_parcial != "N/A") %>%
  mutate(resultado_sentenca_parcial = fct_infreq(resultado_sentenca_parcial) %>%
           fct_relevel("N/D", after = Inf) %>%
           fct_rev()) %>% 
  group_by(uf_arbitragem, resultado_sentenca_parcial) %>% 
  count_and_perc(add_label = T) %>% 
  ggplot(aes(y = resultado_sentenca_parcial, x = n, label = lbl,
             fill = uf_arbitragem, color = uf_arbitragem)) +
  geom_col() +
  geom_label(aes(x = n+1.6), fill = "white", size = 3) +
  scale_fill_manual(values = pal(4)) +
  scale_color_manual(values = pal(4)) +
  scale_x_continuous(breaks = seq(0, 10, by = 2)) +
  labs(title = "Resultado da sentença parcial",
       subtitle = subtitle,
       x = "Número de arbitragens",
       y = "Resultado da sentença parcial") +
  facet_wrap(~uf_arbitragem) +
  theme(legend.position = "none")

save_plot("8_resultado_sentenca_parcial_facet.png")

### Liminar Pré-Arbitral ----

df %>%
  group_by(teve_liminar_pre_arbitral) %>%
  count_and_perc() %>%
  export_table("9_liminar_pre_arbitral.csv")

df %>%
  group_by(teve_liminar_pre_arbitral) %>%
  count_and_perc(add_label = T) %>%
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

df %>%
  ggplot(aes(x = teve_liminar_pre_arbitral, fill = uf_arbitragem)) +
  geom_bar() +
  scale_fill_manual(values = pal(4)) +
  labs(title = "Houve liminar pré-arbitral?",
       subtitle = subtitle,
       y = "Número de arbitragens",
       x = "Houve liminar pré-arbitral?",
       fill = "Unidade Federativa: ")

save_plot("9_liminar_pre_arbitral_stack.png")

df %>%
  group_by(uf_arbitragem, teve_liminar_pre_arbitral) %>%
  count_and_perc(add_label = TRUE) %>%
  ggplot(aes(x = teve_liminar_pre_arbitral, y = n, fill = uf_arbitragem, 
             color = uf_arbitragem, label = lbl)) +
  geom_col(color = "transparent", position = "dodge") +
  geom_label(aes(y = n+0.6), fill = "white", size = 3,
             position = position_dodge(width = .9)) +
  scale_fill_manual(values = pal(4)) +
  scale_color_manual(values = pal(4)) +
  guides(colour = "none") +
  labs(title = "Houve liminar pré-arbitral?",
       subtitle = subtitle,
       y = "Número de arbitragens",
       x = "Houve liminar pré-arbitral?",
       fill = "Unidade Federativa: ")

save_plot("9_liminar_pre_arbitral_dodge.png")

df %>%
  group_by(uf_arbitragem, teve_liminar_pre_arbitral) %>%
  count_and_perc(add_label = TRUE) %>%
  ggplot(aes(x = teve_liminar_pre_arbitral, y = n, fill = uf_arbitragem, 
             color = uf_arbitragem, label = lbl)) +
  geom_col() +
  geom_label(aes(y = n+0.6), fill = "white") +
  scale_fill_manual(values = pal(4)) +
  scale_color_manual(values = pal(4)) +
  labs(title = "Houve liminar pré-arbitral?",
       subtitle = subtitle,
       y = "Número de arbitragens",
       x = "Houve liminar pré-arbitral?",
       fill = "Unidade Federativa: ") +
  theme(legend.position = "none") +
  facet_wrap(~uf_arbitragem)

save_plot("9_liminar_pre_arbitral_facet.png")

### Intevenção TCU ----

df %>%
  group_by(houve_intervencao_tcu) %>%
  count_and_perc() %>%
  export_table("10_intervencao_tcu.csv")

df %>%
  group_by(houve_intervencao_tcu) %>%
  count_and_perc(add_label = T) %>%
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

df %>%
  ggplot(aes(x = houve_intervencao_tcu, fill = uf_arbitragem)) +
  geom_bar() +
  scale_fill_manual(values = pal(4)) +
  labs(title = "Houve intervenção do TCU?",
       subtitle = subtitle,
       y = "Número de arbitragens",
       x = "Houve intervenção do TCU?",
       fill = "Unidade Federativa: ")

save_plot("10_intervencao_tcu_stack.png")

df %>%
  group_by(uf_arbitragem, houve_intervencao_tcu) %>%
  count_and_perc(add_label = TRUE) %>%
  ggplot(aes(x = houve_intervencao_tcu, y = n, fill = uf_arbitragem, 
             color = uf_arbitragem, label = lbl)) +
  geom_col(color = "transparent", position = "dodge") +
  geom_label(aes(y = n+0.8), fill = "white", size = 3,
             position = position_dodge(width = .9)) +
  scale_fill_manual(values = pal(4)) +
  scale_color_manual(values = pal(4)) +
  guides(colour = "none") +
  labs(title = "Houve intervenção do TCU?",
       subtitle = subtitle,
       y = "Número de arbitragens",
       x = "Houve intervenção do TCU?",
       fill = "Unidade Federativa: ")

save_plot("10_intervencao_tcu_dodge.png")

df %>%
  group_by(uf_arbitragem, houve_intervencao_tcu) %>%
  count_and_perc(add_label = TRUE) %>%
  ggplot(aes(x = houve_intervencao_tcu, y = n, fill = uf_arbitragem, 
             color = uf_arbitragem, label = lbl)) +
  geom_col() +
  geom_label(aes(y = n+0.9), fill = "white") +
  scale_fill_manual(values = pal(4)) +
  scale_color_manual(values = pal(4)) +
  labs(title = "Houve intervenção do TCU?",
       subtitle = subtitle,
       y = "Número de arbitragens",
       x = "Houve intervenção do TCU?",
       fill = "Unidade Federativa: ") +
  theme(legend.position = "none") +
  facet_wrap(~uf_arbitragem)

save_plot("10_intervencao_tcu_facet.png")

### Intevenção de terceiros ----

df %>%
  group_by(houve_intervencao_terceiros) %>%
  count_and_perc() %>%
  export_table("11_intervencao_terceiros.csv")

df %>%
  group_by(houve_intervencao_terceiros) %>%
  count_and_perc(add_label = T) %>%
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

df %>%
  ggplot(aes(x = houve_intervencao_terceiros, fill = uf_arbitragem)) +
  geom_bar() +
  scale_fill_manual(values = pal(4)) +
  labs(title = "Houve intervenção de terceiros?",
       subtitle = subtitle,
       y = "Número de arbitragens",
       x = "Houve intervenção de terceiros?",
       fill = "Unidade Federativa: ")

save_plot("11_intervencao_terceiros_stack.png")

df %>%
  group_by(uf_arbitragem, houve_intervencao_terceiros) %>%
  count_and_perc(add_label = TRUE) %>%
  ggplot(aes(x = houve_intervencao_terceiros, y = n, fill = uf_arbitragem, 
             color = uf_arbitragem, label = lbl)) +
  geom_col(color = "transparent", position = "dodge") +
  geom_label(aes(y = n+0.8), fill = "white", size = 3,
             position = position_dodge(width = .9)) +
  scale_fill_manual(values = pal(4)) +
  scale_color_manual(values = pal(4)) +
  guides(colour = "none") +
  labs(title = "Houve intervenção de terceiros?",
       subtitle = subtitle,
       y = "Número de arbitragens",
       x = "Houve intervenção de terceiros?",
       fill = "Unidade Federativa: ")

save_plot("11_intervencao_terceiros_dodge.png")

df %>%
  group_by(uf_arbitragem, houve_intervencao_terceiros) %>%
  count_and_perc(add_label = TRUE) %>%
  ggplot(aes(x = houve_intervencao_terceiros, y = n, fill = uf_arbitragem, 
             color = uf_arbitragem, label = lbl)) +
  geom_col() +
  geom_label(aes(y = n+0.9), fill = "white") +
  scale_fill_manual(values = pal(4)) +
  scale_color_manual(values = pal(4)) +
  labs(title = "Houve intervenção de terceiros?",
       subtitle = subtitle,
       y = "Número de arbitragens",
       x = "Houve intervenção de terceiros?") +
  theme(legend.position = "none") +
  facet_wrap(~uf_arbitragem)

save_plot("11_intervencao_terceiros_facet.png")

### Precatórios ----

df %>%
  group_by(caiu_em_precatorio) %>%
  count_and_perc() %>%
  export_table("12_precatorios.csv")

df %>%
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

df %>%
  ggplot(aes(x = caiu_em_precatorio, fill = uf_arbitragem)) +
  geom_bar() +
  scale_fill_manual(values = pal(4)) +
  labs(title = "Caiu em precatório?",
       subtitle = subtitle,
       y = "Número de arbitragens",
       x = "Caiu em precatório?",
       fill = "Unidade Federativa: ")

save_plot("12_precatorios_stack.png")

df %>%
  group_by(uf_arbitragem, caiu_em_precatorio) %>%
  count_and_perc(add_label = TRUE) %>%
  ggplot(aes(x = caiu_em_precatorio, y = n, fill = uf_arbitragem, 
             color = uf_arbitragem, label = lbl)) +
  geom_col(color = "transparent", position = "dodge") +
  geom_label(aes(y = n+0.8), fill = "white", size = 3,
             position = position_dodge(width = .9)) +
  scale_fill_manual(values = pal(4)) +
  scale_color_manual(values = pal(4)) +
  guides(colour = "none") +
  labs(title = "Caiu em precatório?",
       subtitle = subtitle,
       y = "Número de arbitragens",
       x = "Caiu em precatório?",
       fill = "Unidade Federativa: ")

save_plot("12_precatorios_dodge.png")

df %>%
  group_by(uf_arbitragem, caiu_em_precatorio) %>%
  count_and_perc(add_label = TRUE) %>%
  ggplot(aes(x = caiu_em_precatorio, y = n, fill = uf_arbitragem, 
             color = uf_arbitragem, label = lbl)) +
  geom_col() +
  geom_label(aes(y = n+0.9), fill = "white", size = 3) +
  scale_fill_manual(values = pal(4)) +
  scale_color_manual(values = pal(4)) +
  labs(title = "Caiu em precatório?",
       subtitle = subtitle,
       y = "Número de arbitragens",
       x = "Caiu em precatório?") +
  theme(legend.position = "none") +
  facet_wrap(~uf_arbitragem)

save_plot("12_precatorios_facet.png")

### Variáveis binárias consolidadas ----

var_binarias <- c(
  "houve_reconvencao",
  "teve_sentenca_parcial",
  "teve_liminar_pre_arbitral",
  "houve_intervencao_tcu",
  "houve_intervencao_terceiros",
  "caiu_em_precatorio"
)

perguntas <- c(
  "Houve liminar pré-arbitral?",
  "Houve reconvenção?",
  "Houve sentença parcial?",
  "Houve intervenção de terceiros?",
  "Houve intervenção do TCU?",
  "Caiu em precatório?"
)

df %>%
  select(all_of(var_binarias)) %>% 
  pivot_longer(cols = var_binarias) %>% 
  mutate(
    variavel = case_when(
      name == var_binarias[1] ~ perguntas[2],
      name == var_binarias[2] ~ perguntas[3],
      name == var_binarias[3] ~ perguntas[1],
      name == var_binarias[4] ~ perguntas[5],
      name == var_binarias[5] ~ perguntas[4],
      name == var_binarias[6] ~ perguntas[6]
    ) %>% factor(ordered = T, levels = perguntas)
  ) %>% 
  group_by(variavel, value) %>%
  count_and_perc(add_label = TRUE) %>%
  mutate(fill_case = value %in% c("Sim", "Não")) %>% 
  ggplot(aes(x = value, y = n, fill = fill_case, 
             color = fill_case, label = lbl)) +
  geom_col() +
  geom_label(nudge_y = 1.4, fill = "white", size = 3) +
  scale_fill_manual(values = c("darkgray", single)) +
  scale_color_manual(values = c("darkgray", single)) +
  labs(title = "Características das arbitragens analisadas",
       subtitle = subtitle,
       y = "Número de arbitragens",
       x = "Resposta") +
  theme(legend.position = "none") +
  facet_wrap(~variavel)

save_plot("13_vars_binarias_facet_consolidado.png")

df_vars_binarias <- tibble()

for (i in seq_along(var_binarias)) {
  for (j in seq_along(var_binarias)) {
    df_vars_binarias <- bind_rows(
      df_vars_binarias,
      tibble(
        var1 = var_binarias[i],
        var2 = var_binarias[j]
      )
    )
  }
}

title_varbins <- c(
  "Reconvenção",
  "Sentença Parcial",
  "Liminar Pré-Arbitral",
  "Invertenção do TCU",
  "Intervenção de Terceiros",
  "Precatórios"
)

df_vars_binarias <- filter(df_vars_binarias, var1 != var2) %>% 
  mutate(
    var1_pergunta = case_when(
      var1 == var_binarias[1] ~ perguntas[2],
      var1 == var_binarias[2] ~ perguntas[3],
      var1 == var_binarias[3] ~ perguntas[1],
      var1 == var_binarias[4] ~ perguntas[5],
      var1 == var_binarias[5] ~ perguntas[4],
      var1 == var_binarias[6] ~ perguntas[6]
    ),
    var2_pergunta = case_when(
      var2 == var_binarias[1] ~ perguntas[2],
      var2 == var_binarias[2] ~ perguntas[3],
      var2 == var_binarias[3] ~ perguntas[1],
      var2 == var_binarias[4] ~ perguntas[5],
      var2 == var_binarias[5] ~ perguntas[4],
      var2 == var_binarias[6] ~ perguntas[6]
    ),
    var1_title = case_when(
      var1 == var_binarias[1] ~ title_varbins[1],
      var1 == var_binarias[2] ~ title_varbins[2],
      var1 == var_binarias[3] ~ title_varbins[3],
      var1 == var_binarias[4] ~ title_varbins[4],
      var1 == var_binarias[5] ~ title_varbins[5],
      var1 == var_binarias[6] ~ title_varbins[6]
    ),
    var2_title = case_when(
      var2 == var_binarias[1] ~ title_varbins[1],
      var2 == var_binarias[2] ~ title_varbins[2],
      var2 == var_binarias[3] ~ title_varbins[3],
      var2 == var_binarias[4] ~ title_varbins[4],
      var2 == var_binarias[5] ~ title_varbins[5],
      var2 == var_binarias[6] ~ title_varbins[6]
    )
  )

colors_bin <- list(
  "#00366C", "#005B9E", "#4A83BA", "#9BB3D4"
)

names(colors_bin) <- categorias_sim_nao

for (i in 1:nrow(df_vars_binarias)) {
  pergunta_1 <- df_vars_binarias$var1_pergunta[[i]]
  pergunta_2 <- df_vars_binarias$var2_pergunta[[i]]
  var_1 <- df_vars_binarias$var1[[i]]
  var_2 <- df_vars_binarias$var2[[i]]
  
  df_to_plot <- tibble(
    var1 = df[[var_1]],
    var2 = df[[var_2]]
  )
  
  case_name <- paste0("13.", i, "_vars_bincross_", var_1, "_", var_2)
  plot_name_stack <- paste0(case_name, "_stack.png")
  plot_name_dodge <- paste0(case_name, "_dodge.png")
  plot_title <- paste(df_vars_binarias$var1_title[[i]], "X", df_vars_binarias$var2_title[[i]])
  table_name <- paste0(case_name, ".csv")
  
  vars <- c("var1", "var2")
  names(vars) <- c(var_1, var_2)
  
  var2_cats <- sort(unique(df_to_plot$var2))
  palbin <- NULL
  for (j in seq_along(var2_cats)) {
    cat2_bin <- var2_cats[[j]]
    catcolor <- colors_bin[[cat2_bin]]
    palbin <- c(palbin, catcolor)
  }
  
  df_to_plot %>%
    group_by(var1, var2) %>%
    count_and_perc() %>%
    rename(all_of(vars)) %>% 
    export_table(table_name, folder = "./2024_07_12/dados/13_vars_bincross/")
  
  p1 <- df_to_plot %>%
    ggplot(aes(x = var1, fill = var2)) +
    geom_bar() +
    labs(title = plot_title,
         subtitle = subtitle,
         x = df_vars_binarias$var1_pergunta[[i]],
         fill = df_vars_binarias$var2_pergunta[[i]],
         y = "Número de arbitragens") +
    scale_fill_manual(values = palbin) +
    theme(legend.position = "right")
  
  save_plot(plot_name_stack, path = "./2024_07_12/plots/13_vars_bincross/",
            plot = p1)
  
  p2 <- df_to_plot %>%
    group_by(var2, var1) %>% 
    count_and_perc(add_label = T) %>% 
    ggplot(aes(x = var1, y = n, fill = var2, 
               color = var2, label = lbl)) +
    geom_col(color = "transparent", position = "dodge") +
    geom_label(aes(y = n+0.8), fill = "white", size = 2.4,
               position = position_dodge(width = .9)) +
    scale_fill_manual(values = palbin) +
    scale_color_manual(values = palbin) +
    guides(colour = "none") +
    labs(title = plot_title,
         subtitle = subtitle,
         x = df_vars_binarias$var1_pergunta[[i]],
         fill = df_vars_binarias$var2_pergunta[[i]],
         y = "Número de arbitragens") +
    theme(legend.position = "right")
  
  save_plot(plot_name_dodge, path = "./2024_07_12/plots/13_vars_bincross/",
            plot = p2)
  
}

### Tempo termo ----

df %>%
  mutate(dias_ate_termo = map2_dbl(data_inicio, data_termo, calc_date_dist)) %>%
  filter(!is.na(dias_ate_termo)) %>%
  group_by(uf_arbitragem) %>%
  summarise(
    n = n(),
    media_dias_ate_termo = mean(dias_ate_termo),
    mediana_dias_ate_termo = median(dias_ate_termo),
    minimo_dias_ate_termo = min(dias_ate_termo),
    maximo_dias_ate_termo = max(dias_ate_termo),
    desvio_padrao_dias_ate_termo = sd(dias_ate_termo),
  ) %>%
  mutate(perc = n / sum(n)) %>% 
  ungroup() %>% 
  relocate(perc, .after = n) %>% 
  export_table("14_tempo_termo_uf.csv")

df %>%
  mutate(dias_ate_termo = map2_dbl(data_inicio, data_termo, calc_date_dist)) %>%
  ggplot(aes(x = uf_arbitragem, y = dias_ate_termo)) +
  geom_boxplot(fill = single) +
  scale_y_continuous(limits = c(0, 750)) +
  labs(title = "Tempo até o termo arbitral, por Unidade Federativa",
       subtitle = subtitle,
       x = "Unidade Federativa",
       y = "Tempo, em dias, até o termo arbitral")

save_plot("14_tempo_termo_uf.png")

df %>%
  mutate(dias_ate_termo = map2_dbl(data_inicio, data_termo, calc_date_dist)) %>%
  filter(!is.na(dias_ate_termo)) %>%
  group_by(periodo_inicio) %>%
  summarise(
    n = n(),
    media_dias_ate_termo = mean(dias_ate_termo),
    mediana_dias_ate_termo = median(dias_ate_termo),
    minimo_dias_ate_termo = min(dias_ate_termo),
    maximo_dias_ate_termo = max(dias_ate_termo),
    desvio_padrao_dias_ate_termo = sd(dias_ate_termo)
  ) %>%
  mutate(perc = n / sum(n)) %>% 
  ungroup() %>% 
  relocate(perc, .after = n) %>% 
  export_table("14_tempo_termo_periodo.csv")

df %>%
  mutate(dias_ate_termo = map2_dbl(data_inicio, data_termo, calc_date_dist)) %>%
  ggplot(aes(x = periodo_inicio, y = dias_ate_termo)) +
  geom_boxplot(fill = single) +
  scale_y_continuous(limits = c(0, 750)) +
  scale_x_discrete(drop = FALSE) +
  labs(title = "Tempo até o termo arbitral, por ano de início da arbitragem",
       subtitle = subtitle,
       x = "Ano de início da arbitragem",
       y = "Tempo, em dias, até o termo arbitral")

save_plot("14_tempo_termo_periodo.png")

df %>%
  mutate(dias_ate_termo = map2_dbl(data_inicio, data_termo, calc_date_dist)) %>%
  filter(!is.na(dias_ate_termo)) %>%
  group_by(ano_inicio) %>%
  summarise(
    n = n(),
    media_dias_ate_termo = mean(dias_ate_termo),
    mediana_dias_ate_termo = median(dias_ate_termo),
    minimo_dias_ate_termo = min(dias_ate_termo),
    maximo_dias_ate_termo = max(dias_ate_termo),
    desvio_padrao_dias_ate_termo = sd(dias_ate_termo),
  ) %>%
  mutate(perc = n / sum(n)) %>% 
  ungroup() %>% 
  relocate(perc, .after = n) %>% 
  export_table("14_tempo_termo_ano.csv")

### Tempo fim ----

df %>%
  mutate(dias_ate_fim = map2_dbl(data_inicio, data_fim, calc_date_dist)) %>%
  filter(!is.na(dias_ate_fim)) %>%
  group_by(uf_arbitragem) %>%
  summarise(
    n = n(),
    media_dias_ate_termo = mean(dias_ate_fim),
    mediana_dias_ate_termo = median(dias_ate_fim),
    minimo_dias_ate_termo = min(dias_ate_fim),
    maximo_dias_ate_termo = max(dias_ate_fim),
    desvio_padrao_dias_ate_termo = sd(dias_ate_fim),
  ) %>%
  mutate(perc = n / sum(n)) %>% 
  ungroup() %>% 
  relocate(perc, .after = n) %>% 
  export_table("15_tempo_fim_uf.csv")

df %>%
  mutate(dias_ate_fim = map2_dbl(data_inicio, data_fim, calc_date_dist)) %>%
  ggplot(aes(x = uf_arbitragem, y = dias_ate_fim)) +
  geom_boxplot(fill = single) +
  scale_y_continuous(limits = c(0, 2100)) +
  labs(title = "Tempo até o fim da arbitragem, por Unidade Federativa",
       subtitle = subtitle,
       x = "Unidade Federativa",
       y = "Tempo, em dias, até o fim da arbitragem")

save_plot("15_tempo_fim_uf.png")

df %>%
  mutate(dias_ate_fim = map2_dbl(data_inicio, data_fim, calc_date_dist)) %>%
  filter(!is.na(dias_ate_fim)) %>%
  group_by(periodo_inicio) %>%
  summarise(
    n = n(),
    media_dias_ate_fim = mean(dias_ate_fim),
    mediana_dias_ate_fim = median(dias_ate_fim),
    minimo_dias_ate_fim = min(dias_ate_fim),
    maximo_dias_ate_fim = max(dias_ate_fim),
    desvio_padrao_dias_ate_fim = sd(dias_ate_fim)
  ) %>%
  mutate(perc = n / sum(n)) %>% 
  ungroup() %>% 
  relocate(perc, .after = n) %>%
  export_table("15_tempo_fim_periodo.csv")

df %>%
  mutate(dias_ate_fim = map2_dbl(data_inicio, data_fim, calc_date_dist)) %>%
  ggplot(aes(x = periodo_inicio, y = dias_ate_fim)) +
  geom_boxplot(fill = single) +
  scale_x_discrete(drop = FALSE) +
  labs(title = "Tempo até o fim da arbitragem, por ano de início da arbitragem",
       subtitle = subtitle,
       x = "Ano de início da arbitragem",
       y = "Tempo, em dias, até o fim da arbitragem")

save_plot("15_tempo_fim_periodo.png")

df %>%
  mutate(dias_ate_fim = map2_dbl(data_inicio, data_fim, calc_date_dist)) %>%
  filter(!is.na(dias_ate_fim)) %>%
  group_by(ano_inicio) %>%
  summarise(
    n = n(),
    media_dias_ate_termo = mean(dias_ate_fim),
    mediana_dias_ate_termo = median(dias_ate_fim),
    minimo_dias_ate_termo = min(dias_ate_fim),
    maximo_dias_ate_termo = max(dias_ate_fim),
    desvio_padrao_dias_ate_termo = sd(dias_ate_fim),
  ) %>%
  mutate(perc = n / sum(n)) %>% 
  ungroup() %>% 
  relocate(perc, .after = n) %>% 
  export_table("15_tempo_fim_ano.csv")

### Tempo inicio desde contrato ----

df %>%
  mutate(dias_ate_inicio_desde_contrato = map2_dbl(inicio_contrato, data_inicio, calc_date_dist)) %>%
  filter(!is.na(dias_ate_inicio_desde_contrato)) %>%
  group_by(uf_arbitragem) %>%
  summarise(
    n = n(),
    media_dias_ate_inicio_desde_contrato = mean(dias_ate_inicio_desde_contrato),
    mediana_dias_ate_inicio_desde_contrato = median(dias_ate_inicio_desde_contrato),
    minimo_dias_ate_inicio_desde_contrato = min(dias_ate_inicio_desde_contrato),
    maximo_dias_ate_inicio_desde_contrato = max(dias_ate_inicio_desde_contrato),
    desvio_padrao_dias_ate_inicio_desde_contrato = sd(dias_ate_inicio_desde_contrato),
  ) %>%
  mutate(perc = n / sum(n)) %>% 
  ungroup() %>% 
  relocate(perc, .after = n) %>%
  export_table("16_tempo_inicio_desde_contrato_uf.csv")

df %>%
  mutate(dias_ate_inicio_desde_contrato = map2_dbl(inicio_contrato, data_inicio, calc_date_dist)) %>%
  ggplot(aes(x = uf_arbitragem, y = dias_ate_inicio_desde_contrato)) +
  geom_boxplot(fill = single) +
  # scale_y_continuous(limits = c(0, 2100)) +
  labs(title = "Tempo desde a celebração contrato até o início da arbitragem, por Unidade Federativa",
       subtitle = subtitle,
       x = "Unidade Federativa",
       y = "Tempo, em dias, desde a celebração contrato até o início da arbitragem")

save_plot("16_tempo_inicio_desde_contrato_uf.png")

df %>%
  mutate(dias_ate_inicio_desde_contrato = map2_dbl(inicio_contrato, data_inicio, calc_date_dist)) %>%
  filter(!is.na(dias_ate_inicio_desde_contrato)) %>%
  group_by(periodo_inicio) %>%
  summarise(
    n = n(),
    media_dias_ate_inicio_desde_contrato = mean(dias_ate_inicio_desde_contrato),
    mediana_dias_ate_inicio_desde_contrato = median(dias_ate_inicio_desde_contrato),
    minimo_dias_ate_inicio_desde_contrato = min(dias_ate_inicio_desde_contrato),
    maximo_dias_ate_inicio_desde_contrato = max(dias_ate_inicio_desde_contrato),
    desvio_padrao_dias_ate_inicio_desde_contrato = sd(dias_ate_inicio_desde_contrato),
  ) %>%
  mutate(perc = n / sum(n)) %>% 
  ungroup() %>% 
  relocate(perc, .after = n) %>%
  export_table("16_tempo_inicio_desde_contrato_periodo.csv")

df %>%
  mutate(dias_ate_inicio_desde_contrato = map2_dbl(inicio_contrato, data_inicio, calc_date_dist)) %>%
  ggplot(aes(x = periodo_inicio, y = dias_ate_inicio_desde_contrato)) +
  geom_boxplot(fill = single) +
  scale_x_discrete(drop = FALSE) +
  labs(title = "Tempo desde a celebração contrato até o início da arbitragem, por Unidade Federativa",
       subtitle = subtitle,
       x = "Ano de início da arbitragem",
       y = "Tempo, em dias, desde a celebração contrato até o início da arbitragem")

save_plot("16_tempo_inicio_desde_contrato_periodo.png")

df %>%
  mutate(dias_ate_inicio_desde_contrato = map2_dbl(inicio_contrato, data_inicio, calc_date_dist)) %>%
  filter(!is.na(dias_ate_inicio_desde_contrato)) %>%
  group_by(ano_inicio) %>%
  summarise(
    n = n(),
    media_dias_ate_inicio_desde_contrato = mean(dias_ate_inicio_desde_contrato),
    mediana_dias_ate_inicio_desde_contrato = median(dias_ate_inicio_desde_contrato),
    minimo_dias_ate_inicio_desde_contrato = min(dias_ate_inicio_desde_contrato),
    maximo_dias_ate_inicio_desde_contrato = max(dias_ate_inicio_desde_contrato),
    desvio_padrao_dias_ate_inicio_desde_contrato = sd(dias_ate_inicio_desde_contrato),
  ) %>%
  mutate(perc = n / sum(n)) %>% 
  ungroup() %>% 
  relocate(perc, .after = n) %>% 
  export_table("16_tempo_inicio_desde_contrato_ano.csv")


### Tempo inicio desde problema ----

df %>%
  mutate(dias_ate_inicio_desde_problema = map2_dbl(data_problema, data_inicio, calc_date_dist)) %>%
  filter(!is.na(dias_ate_inicio_desde_problema)) %>%
  group_by(uf_arbitragem) %>%
  summarise(
    n = n(),
    media_dias_ate_inicio_desde_problema = mean(dias_ate_inicio_desde_problema),
    mediana_dias_ate_inicio_desde_problema = median(dias_ate_inicio_desde_problema),
    minimo_dias_ate_inicio_desde_problema = min(dias_ate_inicio_desde_problema),
    maximo_dias_ate_inicio_desde_problema = max(dias_ate_inicio_desde_problema),
    desvio_padrao_dias_ate_inicio_desde_problema = sd(dias_ate_inicio_desde_problema),
  ) %>%
  mutate(perc = n / sum(n)) %>% 
  ungroup() %>% 
  relocate(perc, .after = n) %>%
  export_table("17_tempo_inicio_desde_problema_uf.csv")

df %>%
  mutate(dias_ate_inicio_desde_problema = map2_dbl(data_problema, data_inicio, calc_date_dist)) %>%
  ggplot(aes(x = uf_arbitragem, y = dias_ate_inicio_desde_problema)) +
  geom_boxplot(fill = single) +
  labs(title = "Tempo desde a ocorrência do problema até o início da arbitragem, por Unidade Federativa",
       subtitle = subtitle,
       x = "Unidade Federativa",
       y = "Tempo, em dias, desde a ocorrência do problema até o início da arbitragem")

save_plot("17_tempo_inicio_desde_problema_uf.png")

df %>%
  mutate(dias_ate_inicio_desde_problema = map2_dbl(data_problema, data_inicio, calc_date_dist)) %>%
  filter(!is.na(dias_ate_inicio_desde_problema)) %>%
  group_by(periodo_inicio) %>%
  summarise(
    n = n(),
    media_dias_ate_inicio_desde_problema = mean(dias_ate_inicio_desde_problema),
    mediana_dias_ate_inicio_desde_problema = median(dias_ate_inicio_desde_problema),
    minimo_dias_ate_inicio_desde_problema = min(dias_ate_inicio_desde_problema),
    maximo_dias_ate_inicio_desde_problema = max(dias_ate_inicio_desde_problema),
    desvio_padrao_dias_ate_inicio_desde_problema = sd(dias_ate_inicio_desde_problema),
  ) %>%
  mutate(perc = n / sum(n)) %>% 
  ungroup() %>% 
  relocate(perc, .after = n) %>%
  export_table("17_tempo_inicio_desde_problema_periodo.csv")

df %>%
  mutate(dias_ate_inicio_desde_problema = map2_dbl(data_problema, data_inicio, calc_date_dist)) %>%
  ggplot(aes(x = periodo_inicio, y = dias_ate_inicio_desde_problema)) +
  geom_boxplot(fill = single) +
  scale_x_discrete(drop = FALSE) +
  labs(title = "Tempo desde a ocorrência do problema até o início da arbitragem, por Unidade Federativa",
       subtitle = subtitle,
       x = "Ano de início da arbitragem",
       y = "Tempo, em dias, desde a ocorrência do problema até o início da arbitragem")

save_plot("17_tempo_inicio_desde_problema_periodo.png")

df %>%
  mutate(dias_ate_inicio_desde_problema = map2_dbl(data_problema, data_inicio, calc_date_dist)) %>%
  filter(!is.na(dias_ate_inicio_desde_problema)) %>%
  group_by(ano_inicio) %>%
  summarise(
    n = n(),
    media_dias_ate_inicio_desde_problema = mean(dias_ate_inicio_desde_problema),
    mediana_dias_ate_inicio_desde_problema = median(dias_ate_inicio_desde_problema),
    minimo_dias_ate_inicio_desde_problema = min(dias_ate_inicio_desde_problema),
    maximo_dias_ate_inicio_desde_problema = max(dias_ate_inicio_desde_problema),
    desvio_padrao_dias_ate_inicio_desde_problema = sd(dias_ate_inicio_desde_problema),
  ) %>%
  mutate(perc = n / sum(n)) %>% 
  ungroup() %>% 
  relocate(perc, .after = n) %>% 
  export_table("17_tempo_inicio_desde_problema_ano.csv")

### Tempo problema desde o contrato ----

df %>%
  mutate(dias_ate_problema_desde_contrato = map2_dbl(inicio_contrato, data_problema, calc_date_dist)) %>%
  filter(!is.na(dias_ate_problema_desde_contrato)) %>%
  group_by(uf_arbitragem) %>%
  summarise(
    n = n(),
    media_dias_ate_problema_desde_contrato = mean(dias_ate_problema_desde_contrato),
    mediana_dias_ate_problema_desde_contrato = median(dias_ate_problema_desde_contrato),
    minimo_dias_ate_problema_desde_contrato = min(dias_ate_problema_desde_contrato),
    maximo_dias_ate_problema_desde_contrato = max(dias_ate_problema_desde_contrato),
    desvio_padrao_dias_ate_problema_desde_contrato = sd(dias_ate_problema_desde_contrato),
  ) %>%
  mutate(perc = n / sum(n)) %>% 
  ungroup() %>% 
  relocate(perc, .after = n) %>% 
  export_table("18_tempo_problema_desde_contrato_uf.csv")

df %>%
  mutate(dias_ate_problema_desde_contrato = map2_dbl(inicio_contrato, data_problema, calc_date_dist)) %>%
  ggplot(aes(x = uf_arbitragem, y = dias_ate_problema_desde_contrato)) +
  geom_boxplot(fill = single) +
  labs(title = "Tempo entre a celebração do contrato e a ocorrência do problema, por Unidade Federativa",
       subtitle = subtitle,
       x = "Unidade Federativa",
       y = "Tempo, em dias, desde a celebração do contrato até a ocorrência do problema")

save_plot("18_tempo_problema_desde_contrato_uf.png")

df %>%
  mutate(dias_ate_problema_desde_contrato = map2_dbl(inicio_contrato, data_problema, calc_date_dist)) %>%
  filter(!is.na(dias_ate_problema_desde_contrato)) %>%
  group_by(periodo_inicio) %>%
  summarise(
    n = n(),
    media_dias_ate_problema_desde_contrato = mean(dias_ate_problema_desde_contrato),
    mediana_dias_ate_problema_desde_contrato = median(dias_ate_problema_desde_contrato),
    minimo_dias_ate_problema_desde_contrato = min(dias_ate_problema_desde_contrato),
    maximo_dias_ate_problema_desde_contrato = max(dias_ate_problema_desde_contrato),
    desvio_padrao_dias_ate_problema_desde_contrato = sd(dias_ate_problema_desde_contrato),
  ) %>%
  mutate(perc = n / sum(n)) %>% 
  ungroup() %>% 
  relocate(perc, .after = n) %>%
  export_table("18_tempo_problema_desde_contrato_periodo.csv")

df %>%
  mutate(dias_ate_problema_desde_contrato = map2_dbl(inicio_contrato, data_problema, calc_date_dist)) %>%
  ggplot(aes(x = periodo_inicio, y = dias_ate_problema_desde_contrato)) +
  geom_boxplot(fill = single) +
  scale_x_discrete(drop = FALSE) +
  labs(title = "Tempo entre a celebração do contrato e a ocorrência do problema, por Unidade Federativa",
       subtitle = subtitle,
       x = "Ano de início da arbitragem",
       y = "Tempo, em dias, desde a celebração do contrato até a ocorrência do problema")

save_plot("18_tempo_problema_desde_contrato_periodo.png")

df %>%
  mutate(dias_ate_problema_desde_contrato = map2_dbl(inicio_contrato, data_problema, calc_date_dist)) %>%
  filter(!is.na(dias_ate_problema_desde_contrato)) %>%
  group_by(ano_inicio) %>%
  summarise(
    n = n(),
    media_dias_ate_problema_desde_contrato = mean(dias_ate_problema_desde_contrato),
    mediana_dias_ate_problema_desde_contrato = median(dias_ate_problema_desde_contrato),
    minimo_dias_ate_problema_desde_contrato = min(dias_ate_problema_desde_contrato),
    maximo_dias_ate_problema_desde_contrato = max(dias_ate_problema_desde_contrato),
    desvio_padrao_dias_ate_problema_desde_contrato = sd(dias_ate_problema_desde_contrato),
  ) %>%
  mutate(perc = n / sum(n)) %>% 
  ungroup() %>% 
  relocate(perc, .after = n) %>% 
  export_table("18_tempo_problema_desde_contrato_ano.csv")

### Acordos por ano ----

cats_acordo <- c(
  "Sim",
  "Não",
  "N/A",
  "N/D"
)

df %>%
  mutate(acordo = case_when(
    resultado_final == "Acordo" ~ cats_acordo[1],
    resultado_final == "N/A" ~ cats_acordo[3],
    resultado_final == "N/D" ~ cats_acordo[4],
    TRUE ~ cats_acordo[2]
  ) %>% factor(ordered = T, levels = cats_acordo),
  ano_fim = str_extract(data_fim, "\\d{4}") %>% factor(ordered = T, levels = as.character(2008:2024))) %>%
  filter(!is.na(ano_fim)) %>% 
  ggplot(aes(x = ano_fim, fill = acordo)) +
  geom_bar() +
  scale_fill_manual(values = c(pal(5)[c(1, 4)], "lightgray", "darkgray")) +
  scale_x_discrete(drop = FALSE) +
  labs(title = "Número de acordos, por ano de fim da arbitragem",
       subtitle = subtitle,
       x = "Ano de fim da arbitragem",
       y = "Número de arbitragens encerradas",
       fill = "Terminou em acordo? ") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

save_plot("19_acordos_por_ano.png")


### Valor da causa ----

df_valor_da_causa <- df %>% 
  mutate(valor_causa_numerico = as.numeric(valor_da_causa)) %>% 
  mutate(valor_da_causa = case_when(
    !is.na(valor_causa_numerico) ~ valor_da_causa,
    valor_da_causa == "N/D" ~ valor_da_causa,
    TRUE ~ valor_da_causa %>%
      str_remove(",.+") %>% 
      str_remove_all("\\.") %>% 
      str_extract("\\d+")
  )) %>% 
  mutate(valor_causa_numerico = as.numeric(valor_da_causa)) %>% 
  filter(!is.na(valor_causa_numerico))

df_valor_da_causa %>% 
  filter(setor != "N/A") %>%   
  mutate(setor = fct_lump_min(setor, 2, other_level = "Outros Setores") %>% 
           fct_reorder(valor_causa_numerico) %>% 
           fct_relevel("Outros Setores", after = 0)) %>% 
  ggplot(aes(y = setor, x = valor_causa_numerico / 1000000)) +
  geom_boxplot(fill = single) +
  labs(title = "Valores das causas, por setor",
       subtitle = subtitle,
       y = "Setor", x = "Valor da causa (Milhões de R$)")

save_plot("20_valor_setor_todos.png")

df_valor_da_causa %>% 
  filter(setor != "N/A" & setor != "Telecomunicações") %>%   
  mutate(setor = fct_lump_min(setor, 2, other_level = "Outros Setores") %>% 
           fct_reorder(valor_causa_numerico) %>% 
           fct_relevel("Outros Setores", after = 0)) %>% 
  ggplot(aes(y = setor, x = valor_causa_numerico / 1000000)) +
  geom_boxplot(fill = single) +
  labs(title = "Valores das causas, por setor, excluindo telecomunicações",
       subtitle = subtitle,
       y = "Setor (exceto Telecomunicações)", x = "Valor da causa (Milhões de R$)")

save_plot("20_valor_setor_exceto_telecom.png")

n_telecom <- df_valor_da_causa %>% 
  filter(setor != "N/A") %>%   
  filter(setor == "Telecomunicações") %>% 
  nrow()

n_demais <- df_valor_da_causa %>% 
  filter(setor != "N/A") %>%   
  filter(setor != "Telecomunicações") %>% 
  nrow()

df_valor_da_causa %>% 
  filter(setor != "N/A") %>%   
  mutate(setor = ifelse(setor == "Telecomunicações",
                        paste0(setor, " (N = ", n_telecom, ")"),
                        paste0("Demais Setores (N = ", n_demais, ")"))) %>% 
  ggplot(aes(x = setor, y = valor_causa_numerico / 1000000)) +
  geom_boxplot(fill = single, alpha = 0.1) +
  geom_jitter(color = single, alpha = 0.8) +
  labs(title = "Valor da causa: setor de telecomunicações VS. Demais Setores",
       subtitle = subtitle,
       y = "Setor", x = "Valor da causa (Milhões de R$)")

save_plot("20_valor_telecom_x_demais.png")



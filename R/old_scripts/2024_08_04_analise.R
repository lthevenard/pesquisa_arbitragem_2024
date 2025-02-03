# BIBLIOTECAS ----

library(tidyverse)
library(lubridate)
library(readxl)

# ATUALIZAR ANÁLISE APENAS COM ARBITRAGENS ----

## Functions ----

save_plot <- function(filename, path = "./2024_08_04/output/plots", height = 6, width = 10, dpi = 300, ...) {
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

export_table <- function(df, filename, folder = "./2024_08_04/output/dados/") {
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

rm_accent <- function(str,pattern="all") {
  # Rotinas e funções úteis V 1.0
  # rm.accent - REMOVE ACENTOS DE PALAVRAS
  # Função que tira todos os acentos e pontuações de um vetor de strings.
  # Parâmetros:
  # str - vetor de strings que terão seus acentos retirados.
  # patterns - vetor de strings com um ou mais elementos indicando quais acentos deverão ser retirados.
  #            Para indicar quais acentos deverão ser retirados, um vetor com os símbolos deverão ser passados.
  #            Exemplo: pattern = c("´", "^") retirará os acentos agudos e circunflexos apenas.
  #            Outras palavras aceitas: "all" (retira todos os acentos, que são "´", "`", "^", "~", "¨", "ç")
  if(!is.character(str))
    str <- as.character(str)
  
  pattern <- unique(pattern)
  
  if(any(pattern=="Ç"))
    pattern[pattern=="Ç"] <- "ç"
  
  symbols <- c(
    acute = "áéíóúÁÉÍÓÚýÝ",
    grave = "àèìòùÀÈÌÒÙ",
    circunflex = "âêîôûÂÊÎÔÛ",
    tilde = "ãõÃÕñÑ",
    umlaut = "äëïöüÄËÏÖÜÿ",
    cedil = "çÇ"
  )
  
  nudeSymbols <- c(
    acute = "aeiouAEIOUyY",
    grave = "aeiouAEIOU",
    circunflex = "aeiouAEIOU",
    tilde = "aoAOnN",
    umlaut = "aeiouAEIOUy",
    cedil = "cC"
  )
  
  accentTypes <- c("´","`","^","~","¨","ç")
  
  if(any(c("all","al","a","todos","t","to","tod","todo")%in%pattern)) # opcao retirar todos
    return(chartr(paste(symbols, collapse=""), paste(nudeSymbols, collapse=""), str))
  
  for(i in which(accentTypes%in%pattern))
    str <- chartr(symbols[i],nudeSymbols[i], str)
  
  return(str)
}

## Data ----

df_arbitragens <- read_xlsx("2024_08_04/input/df.xlsx", sheet = "arbitragens", col_types = rep("text", 31)) %>% 
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

df_arbitragens <- df_arbitragens %>%
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

count(df_arbitragens, valor_da_causa) %>% View()

glimpse(df_arbitragens)

## Analysis ----

theme_set({{theme_bw() + theme(legend.position = "bottom")}})

pal <- colorspace::choose_palette()
single <- pal(5)[2]

subtitle <- "Pesquisa de arbitragens (2002-2022)"

### Universo analisado, por ano ----

df_arbitragens %>%
  group_by(periodo_inicio, uf_arbitragem) %>%
  count_and_perc() %>% 
  arrange(periodo_inicio, uf_arbitragem) %>%
  export_table("1_universo_periodo.csv")

df_arbitragens %>%
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

df_arbitragens %>%
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

df_arbitragens %>%
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

df_arbitragens %>%
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

df_arbitragens %>%
  group_by(camara) %>%
  count_and_perc() %>% 
  arrange(camara) %>% 
  export_table("2_camaras.csv")

df_arbitragens %>%
  ggplot(aes(x = cam_class, fill = uf_arbitragem)) +
  geom_bar() +
  scale_fill_manual(values = pal(4)) +
  labs(title = "Câmaras das arbitragens, por Unidade Federativa",
       subtitle = subtitle,
       y = "Número de arbitragens identificadas",
       x = "Câmaras", fill = "Unidade Federativa: ")

save_plot("2_camaras_uf_stack.png")

df_arbitragens %>%
  ggplot(aes(x = cam_class, fill = uf_arbitragem)) +
  geom_bar(position = "dodge") +
  scale_fill_manual(values = pal(4)) +
  labs(title = "Câmaras das arbitragens, por Unidade Federativa",
       subtitle = subtitle,
       y = "Número de arbitragens identificadas",
       x = "Câmaras", fill = "Unidade Federativa: ")

save_plot("2_camaras_uf_dodge.png")

df_arbitragens %>%
  ggplot(aes(fill = cam_class, x = uf_arbitragem)) +
  geom_bar() +
  scale_fill_manual(values = c(pal(5)[3:2], "darkgray", "lightgray")) +
  labs(title = "Câmaras das arbitragens, por Unidade Federativa",
       subtitle = subtitle,
       y = "Número de arbitragens identificadas",
       x = "Unidade Federativa", fill = "Câmaras: ")

save_plot("2_uf_camaras_stack.png")

df_arbitragens %>%
  ggplot(aes(fill = cam_class, x = uf_arbitragem)) +
  geom_bar(position = "dodge") +
  scale_fill_manual(values = c(pal(5)[3:2], "darkgray", "lightgray")) +
  labs(title = "Câmaras das arbitragens, por Unidade Federativa",
       subtitle = subtitle,
       y = "Número de arbitragens identificadas",
       x = "Unidade Federativa", fill = "Câmaras: ")

save_plot("2_uf_camaras_dodge.png")

df_arbitragens %>%
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

n_arbitragens <- nrow(df_arbitragens)

df_objetos <- df_arbitragens %>% 
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

df_resultado_final <- df_arbitragens %>%
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

df_arbitragens %>%
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

df_setor <- df_arbitragens %>%
  filter(setor != "N/A") %>%   
  mutate(setor = fct_lump_min(setor, 2, other_level = "Outros Setores") %>% 
           fct_infreq())

df_arbitragens %>%
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

df_arbitragens %>%
  group_by(houve_reconvencao) %>%
  count_and_perc() %>%
  export_table("6_reconvencao.csv")

df_arbitragens %>%
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

df_arbitragens %>%
  ggplot(aes(x = houve_reconvencao, fill = uf_arbitragem)) +
  geom_bar() +
  scale_fill_manual(values = pal(4)) +
  labs(title = "Houve reconvenção?",
       subtitle = subtitle,
       y = "Número de arbitragens",
       x = "Houve reconvenção?",
       fill = "Unidade Federativa: ")

save_plot("6_reconvencao_stack.png")

df_arbitragens %>%
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

df_arbitragens %>%
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

df_arbitragens %>%
  group_by(teve_sentenca_parcial) %>%
  count_and_perc() %>%
  export_table("7_sentenca_parcial.csv")

df_arbitragens %>%
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

df_arbitragens %>%
  ggplot(aes(x = teve_sentenca_parcial, fill = uf_arbitragem)) +
  geom_bar() +
  scale_fill_manual(values = pal(4)) +
  labs(title = "Houve sentença parcial?",
       subtitle = subtitle,
       y = "Número de arbitragens",
       x = "Houve sentença parcial?",
       fill = "Unidade Federativa: ")

save_plot("7_sentenca_parcial_stack.png")

df_arbitragens %>%
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

df_arbitragens %>%
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

df_resultado_parcial <- df_arbitragens %>%
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

df_arbitragens %>%
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

df_arbitragens %>%
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

df_arbitragens %>%
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

df_arbitragens %>%
  group_by(teve_liminar_pre_arbitral) %>%
  count_and_perc() %>%
  export_table("9_liminar_pre_arbitral.csv")

df_arbitragens %>%
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

df_arbitragens %>%
  ggplot(aes(x = teve_liminar_pre_arbitral, fill = uf_arbitragem)) +
  geom_bar() +
  scale_fill_manual(values = pal(4)) +
  labs(title = "Houve liminar pré-arbitral?",
       subtitle = subtitle,
       y = "Número de arbitragens",
       x = "Houve liminar pré-arbitral?",
       fill = "Unidade Federativa: ")

save_plot("9_liminar_pre_arbitral_stack.png")

df_arbitragens %>%
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

df_arbitragens %>%
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

df_arbitragens %>%
  group_by(houve_intervencao_tcu) %>%
  count_and_perc() %>%
  export_table("10_intervencao_tcu.csv")

df_arbitragens %>%
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

df_arbitragens %>%
  ggplot(aes(x = houve_intervencao_tcu, fill = uf_arbitragem)) +
  geom_bar() +
  scale_fill_manual(values = pal(4)) +
  labs(title = "Houve intervenção do TCU?",
       subtitle = subtitle,
       y = "Número de arbitragens",
       x = "Houve intervenção do TCU?",
       fill = "Unidade Federativa: ")

save_plot("10_intervencao_tcu_stack.png")

df_arbitragens %>%
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

df_arbitragens %>%
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

df_arbitragens %>%
  group_by(houve_intervencao_terceiros) %>%
  count_and_perc() %>%
  export_table("11_intervencao_terceiros.csv")

df_arbitragens %>%
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

df_arbitragens %>%
  ggplot(aes(x = houve_intervencao_terceiros, fill = uf_arbitragem)) +
  geom_bar() +
  scale_fill_manual(values = pal(4)) +
  labs(title = "Houve intervenção de terceiros?",
       subtitle = subtitle,
       y = "Número de arbitragens",
       x = "Houve intervenção de terceiros?",
       fill = "Unidade Federativa: ")

save_plot("11_intervencao_terceiros_stack.png")

df_arbitragens %>%
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

df_arbitragens %>%
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

df_arbitragens %>%
  group_by(caiu_em_precatorio) %>%
  count_and_perc() %>%
  export_table("12_precatorios.csv")

df_arbitragens %>%
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

df_arbitragens %>%
  ggplot(aes(x = caiu_em_precatorio, fill = uf_arbitragem)) +
  geom_bar() +
  scale_fill_manual(values = pal(4)) +
  labs(title = "Caiu em precatório?",
       subtitle = subtitle,
       y = "Número de arbitragens",
       x = "Caiu em precatório?",
       fill = "Unidade Federativa: ")

save_plot("12_precatorios_stack.png")

df_arbitragens %>%
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

df_arbitragens %>%
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

df_arbitragens %>%
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
    var1 = df_arbitragens[[var_1]],
    var2 = df_arbitragens[[var_2]]
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
    export_table(table_name, folder = "./2024_08_04/output/dados/13_vars_bincross/")
  
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
  
  save_plot(plot_name_stack, path = "./2024_08_04/output/plots/13_vars_bincross/",
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
  
  save_plot(plot_name_dodge, path = "./2024_08_04/output/plots/13_vars_bincross/",
            plot = p2)
  
}

### Tempo termo ----

df_arbitragens %>%
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

df_arbitragens %>%
  mutate(dias_ate_termo = map2_dbl(data_inicio, data_termo, calc_date_dist)) %>%
  ggplot(aes(x = uf_arbitragem, y = dias_ate_termo)) +
  geom_boxplot(fill = single) +
  scale_y_continuous(limits = c(0, 750)) +
  labs(title = "Tempo até o termo arbitral, por Unidade Federativa",
       subtitle = subtitle,
       x = "Unidade Federativa",
       y = "Tempo, em dias, até o termo arbitral")

save_plot("14_tempo_termo_uf.png")

df_arbitragens %>%
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

df_arbitragens %>%
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

df_arbitragens %>%
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

df_arbitragens %>%
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

df_arbitragens %>%
  mutate(dias_ate_fim = map2_dbl(data_inicio, data_fim, calc_date_dist)) %>%
  ggplot(aes(x = uf_arbitragem, y = dias_ate_fim)) +
  geom_boxplot(fill = single) +
  scale_y_continuous(limits = c(0, 2100)) +
  labs(title = "Tempo até o fim da arbitragem, por Unidade Federativa",
       subtitle = subtitle,
       x = "Unidade Federativa",
       y = "Tempo, em dias, até o fim da arbitragem")

save_plot("15_tempo_fim_uf.png")

df_arbitragens %>%
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

df_arbitragens %>%
  mutate(dias_ate_fim = map2_dbl(data_inicio, data_fim, calc_date_dist)) %>%
  ggplot(aes(x = periodo_inicio, y = dias_ate_fim)) +
  geom_boxplot(fill = single) +
  scale_x_discrete(drop = FALSE) +
  labs(title = "Tempo até o fim da arbitragem, por ano de início da arbitragem",
       subtitle = subtitle,
       x = "Ano de início da arbitragem",
       y = "Tempo, em dias, até o fim da arbitragem")

save_plot("15_tempo_fim_periodo.png")

df_arbitragens %>%
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

df_arbitragens %>%
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

df_arbitragens %>%
  mutate(dias_ate_inicio_desde_contrato = map2_dbl(inicio_contrato, data_inicio, calc_date_dist)) %>%
  ggplot(aes(x = uf_arbitragem, y = dias_ate_inicio_desde_contrato)) +
  geom_boxplot(fill = single) +
  # scale_y_continuous(limits = c(0, 2100)) +
  labs(title = "Tempo desde a celebração contrato até o início da arbitragem, por Unidade Federativa",
       subtitle = subtitle,
       x = "Unidade Federativa",
       y = "Tempo, em dias, desde a celebração contrato até o início da arbitragem")

save_plot("16_tempo_inicio_desde_contrato_uf.png")

df_arbitragens %>%
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

df_arbitragens %>%
  mutate(dias_ate_inicio_desde_contrato = map2_dbl(inicio_contrato, data_inicio, calc_date_dist)) %>%
  ggplot(aes(x = periodo_inicio, y = dias_ate_inicio_desde_contrato)) +
  geom_boxplot(fill = single) +
  scale_x_discrete(drop = FALSE) +
  labs(title = "Tempo desde a celebração contrato até o início da arbitragem, por Unidade Federativa",
       subtitle = subtitle,
       x = "Ano de início da arbitragem",
       y = "Tempo, em dias, desde a celebração contrato até o início da arbitragem")

save_plot("16_tempo_inicio_desde_contrato_periodo.png")

df_arbitragens %>%
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

df_arbitragens %>%
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

df_arbitragens %>%
  mutate(dias_ate_inicio_desde_problema = map2_dbl(data_problema, data_inicio, calc_date_dist)) %>%
  ggplot(aes(x = uf_arbitragem, y = dias_ate_inicio_desde_problema)) +
  geom_boxplot(fill = single) +
  labs(title = "Tempo desde a ocorrência do problema até o início da arbitragem, por Unidade Federativa",
       subtitle = subtitle,
       x = "Unidade Federativa",
       y = "Tempo, em dias, desde a ocorrência do problema até o início da arbitragem")

save_plot("17_tempo_inicio_desde_problema_uf.png")

df_arbitragens %>%
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

df_arbitragens %>%
  mutate(dias_ate_inicio_desde_problema = map2_dbl(data_problema, data_inicio, calc_date_dist)) %>%
  ggplot(aes(x = periodo_inicio, y = dias_ate_inicio_desde_problema)) +
  geom_boxplot(fill = single) +
  scale_x_discrete(drop = FALSE) +
  labs(title = "Tempo desde a ocorrência do problema até o início da arbitragem, por Unidade Federativa",
       subtitle = subtitle,
       x = "Ano de início da arbitragem",
       y = "Tempo, em dias, desde a ocorrência do problema até o início da arbitragem")

save_plot("17_tempo_inicio_desde_problema_periodo.png")

df_arbitragens %>%
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

df_arbitragens %>%
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

df_arbitragens %>%
  mutate(dias_ate_problema_desde_contrato = map2_dbl(inicio_contrato, data_problema, calc_date_dist)) %>%
  ggplot(aes(x = uf_arbitragem, y = dias_ate_problema_desde_contrato)) +
  geom_boxplot(fill = single) +
  labs(title = "Tempo entre a celebração do contrato e a ocorrência do problema, por Unidade Federativa",
       subtitle = subtitle,
       x = "Unidade Federativa",
       y = "Tempo, em dias, desde a celebração do contrato até a ocorrência do problema")

save_plot("18_tempo_problema_desde_contrato_uf.png")

df_arbitragens %>%
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

df_arbitragens %>%
  mutate(dias_ate_problema_desde_contrato = map2_dbl(inicio_contrato, data_problema, calc_date_dist)) %>%
  ggplot(aes(x = periodo_inicio, y = dias_ate_problema_desde_contrato)) +
  geom_boxplot(fill = single) +
  scale_x_discrete(drop = FALSE) +
  labs(title = "Tempo entre a celebração do contrato e a ocorrência do problema, por Unidade Federativa",
       subtitle = subtitle,
       x = "Ano de início da arbitragem",
       y = "Tempo, em dias, desde a celebração do contrato até a ocorrência do problema")

save_plot("18_tempo_problema_desde_contrato_periodo.png")

df_arbitragens %>%
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

df_arbitragens %>%
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

df_valor_da_causa <- df_arbitragens %>% 
  filter(valor_da_causa != "N/D") %>% 
  mutate(valor_causa_numerico = as.numeric(valor_da_causa))

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

### Sucumbência para o PP ----

uf_simple_counts <- df_arbitragens %>% 
  filter(!(sucumbencia_prevista_para_pp %in% c("N/A", "N/D"))) %>%
  mutate(uf_simple = ifelse(uf_arbitragem == "Federal", "Federal", "SP e RJ")) %>%
  count(uf_simple) %>% 
  mutate(uf_simple_n = paste(uf_simple, " (N = ", n, ")")) %>% 
  select(uf_simple, uf_simple_n)

df_arbitragens %>% 
  filter(!(sucumbencia_prevista_para_pp %in% c("N/A", "N/D"))) %>%
  mutate(sucumbencia_prevista_para_pp = as.numeric(sucumbencia_prevista_para_pp),
         uf_simple = ifelse(uf_arbitragem == "Federal", "Federal", "SP e RJ")) %>%
  left_join(uf_simple_counts, by = "uf_simple") %>% 
  ggplot(aes(x = uf_simple_n, y = sucumbencia_prevista_para_pp)) +
  geom_boxplot(fill = single) +
  labs(title = "O percentual de sucumbência atribuído ao Poder Público foi maior nas arbitragens estaduais",
       subtitle = subtitle,
       x = "Abitragens Federais x Estaduais",
       y = "Percentual da sucumbência atribuída ao Poder Público") +
  scale_y_continuous(labels = scales::label_percent())

save_plot("21_sucumbencia_do_poder_publico.png")

# DOCUMENTOS ----

df_documentos <- read_xlsx("2024_08_04/input/df.xlsx", sheet = "documentos", col_types = c(rep("text", 4), "numeric")) %>% 
  janitor::clean_names() %>% 
  filter(!duplicated(id_documento)) %>% 
  filter(!is.na(tipo_documento))

## Limpeza dos tipos de documentos ----

lista_de_tipos_normalizados <- df_documentos %>% 
  mutate(tipo_documento = str_to_lower(tipo_documento) %>% 
           rm_accent() %>% 
           str_squish()) %>% 
  count(tipo_documento, sort = TRUE) %>%
  .$tipo_documento

limpeza_de_tipos <- list(
  op = list(
    cases = c("ordem procedimental", "anexo ordem processual"),
    convert = "Ordem Procedimental"
  ),
  mn = list(
    cases = c(
      "manifestacao",
      "manifestacao requerida",
      "manifestacao interveniente anomala",
      "manifestacao interveniente nomala",
      "manifestacao requerente",
      "manifestacao sobre ampliacao do pedido",
      "manifestacao sobre cautelar",
      "manifestacao sobre liminar",
      "manifestacaod260"
    ),
    convert = "Manifestação"
  ),
  ai = list(
    cases = c(
      "alegacoes iniciais",
      "alegacoes iniciais cptm",
      "alegacoes iniciais esp",
      "alegacoes iniciais requerente",
      "alegacoes iniciais requerido"
    ),
    convert = "Alegações Iniciais"
  ),
  rp = list(
    cases = c(
      "replica",
      "replica – cptm",
      "replica – esp",
      "replica – requerente",
      "replica e resposta a reconvencao"
    ),
    convert = "Réplica"
  ),
  tp = list(
    cases = c(
      "treplica",
      "treplica – cptm",
      "treplica – esp",
      "treplica – requerente",
      "treplica (cptm)",
      "treplica (estado)",
      "treplica a reconvencao",
      "treplica e replica a resposta a reconvencao"  
    ),
    convert = "Tréplica"
  ),
  rai = list(
    cases = c(
      "resposta as alegacoes iniciais",
      "resposta alegacoes inicias",
      "resposta as alegacoes iniciais – cptm",
      "resposta as alegacoes iniciais – esp",
      "resposta as alegacoes inicias",
      "resposta as alegacoes iniciais – requerente",
      "resposta as alegacoes iniciais (cptm)",
      "resposta as alegacoes iniciais (estado)",
      "resposta as alegacoes iniciais e reconvencao",
      "respostas as alegacoes iniciais"
    ),
    convert = "Resposta às Alegações Iniciais"
  ),
  rreq = list(
    cases = c("resposta ao requerimento", "resposta requerimento arbitragem"),
    convert = "Resposta ao Requerimento"
  ),
  af = list(
    cases = c(
      "alegacoes finais",
      "alegacoes finais requerente",
      "alegacoes finais requerido",
      "alegacoes finais (interveniente anomala)"
    ),
    convert = "Alegações Finais"
  ),
  am = list(
    cases = c(
      "ata de missao",
      "ata de missao antes do aditamento"
    ),
    convert = "Ata de Missão"
  ),
  rarb = list(
    cases = c("requerimento arbitragem", "requerimento de arbitragem"),
    convert = "Requerimento de Arbitragem"
  ),
  ep = list(
    cases = c("especificacao provas"),
    convert = "Especificação de Provas"
  ),
  tarb = list(
    cases = c("termo de arbitragem", "termo arbitragem"),
    convert = "Termo de Arbitragem"
  ),
  sentarb = list(
    cases = c(
      "sentenca arbitral",
      "sentenca",
      "sentenca arbitral final",
      "sentenca final",
      "sentenca arbitral e decisao de esclarecimentos",
      "sentenca e decisao de esclarecimento"
    ),
    convert = "Sentença Arbitral Final"
  ),
  sentpar = list(
    cases = c(
      "sentenca arbitral parcial",
      "sentenca parcial",
      "sentenca parcial e decisao sobre provas"
    ),
    convert = "Sentença Arbitral Parcial"
  ),
  addsent = list(
    cases = c("addendum a sentenca"),
    convert = "Addendum à Sentença"
  ),
  rreqarb = list(
    cases = c(
      "decisao de esclarecimento",
      "decisao pedido de esclarecimento",
      "decisao do pedido de esclarecimentos",
      "decisao em pedido de esclarecimentos",
      "decisao esclarecimentos pos exclusao da unia",
      "decisao sobre pedido de esclarecimentos"
    ),
    convert = "Decisão de Pedido de Esclarecimentos"
  ),
  psus = list(
    cases = c("pedido suspensao"),
    convert = "Pedido de Suspensão"
  ),
  senthomolog = list(
    cases = c("sentenca homologatoria de acordo"),
    convert = "Sentença Homologatória de Acordo"
  ),
  addsentpar = list(
    cases = c("addendum a sentenca parcial"),
    convert = "Addendum à Sentença Parcial"
  ),
  pedrec = list(
    cases = c("pedido de reconsideracao"),
    convert = "Pedido de Reconsideração"
  ),
  comparb = list(
    cases = c("compromisso arbitral", "termo de compromisso arbitral"),
    convert = "Compromisso Arbitral"
  ),
  impugques = list(
    cases = c("impugnacao quesitos"),
    convert = "Impuganação de Quesitos"
  ),
  indiqassques = list(
    cases = c("indicacao de assistentes tecnicos e quesitos"),
    convert = "Indicação de Assistentes Técnicos e Quesitos"
  ),
  pedesc = list(
    cases = c("pedido de esclarecimento"),
    convert = "Pedido de Esclarecimento"
  ),
  resprec = list(
    cases = c("resposta reconvencao"),
    convert = "Resposta à Reconvenção"
  ),
  adita = list(
    cases = c("aditamento da ata de missao", "aditamento do requerimento"),
    convert = "Aditamento"
  ),
  pedcaut = list(
    cases = c("aditamento e pedido de tutela cautelar"),
    convert = "Pedido de Tutela Cautelar"
  ),
  alegfinpar = list(
    cases = c(
      "alegacoes finais parciais",
      "alegacoes finais parciais – cptm",
      "alegacoes finais parciais – esp",
      "alegacoes finais parciais – requerente",
      "alegacoes finais parciais requerente",
      "alegacoes finais parciais requerido"
    ),
    convert = "Alegações Finais Parciais"
  ),
  ataud = list(
    cases = c("ata de audiencia", "audiencia"),
    convert = "Ata de Audiência"
  ),
  decaut = list(
    cases = c("decisao cautelar"),
    convert = "Decisão Cautelar"
  ),
  decemerg = list(
    cases = c("decisao de emergencia"),
    convert = "Decisão de Emergência"
  ),
  decext = list(
    cases = c("decisao extintiva"),
    convert = "Decisão Extintiva"
  ),
  decconfid = list(
    cases = c("decisao sobre confidencialidade"),
    convert = "Decisão sobre Confidencialidade"
  ),
  decurg = list(
    cases = c(
      "decisao sobre revogacao ou manutencao da tutela de urgencia",
      "pedido revogacao liminar"
    ),
    convert = "Decisão sobre Tutela de Urgência"
  ),
  impugpro = list(
    cases = c("impugnacao provas"),
    convert = "Impugnação de Provas"
  ),
  junt = list(
    cases = c("juntada"),
    convert = "Juntada"
  ),
  notific = list(
    cases = c("notificacao sobre requerimento"),
    convert = "Notificação"
  ),
  reqaobj = list(
    cases = c("requerimento ampliacao objetiva"),
    convert = "Requerimento de Ampliação Objetiva"
  ),
  reprevlim = list(
    cases = c("resposa revogacao liminar"),
    convert = "Resposta à Revogação de Liminar"
  ),
  repimpug = list(
    cases = c("resposta a impugnacao", "resposta impugnacao provas"),
    convert = "Resposta a Impugnação"
  ),
  respanp = list(
    cases = c("resposta da anp"),
    convert = "Resposta da ANP"
  ),
  senthomologdesist = list(
    cases = c("sentenca arbitral homologatoria de desistencia"),
    convert = "Sentença Homologatória de Desistência"
  ),
  sentexcuni = list(
    cases = c(
      "sentenca exclui uniao",
      "sentenca que exclui a uniao"
    ),
    convert = "Sentença que Exclui a União"
  )
)

df_documentos <- df_documentos %>% 
  mutate(tipo_documento = str_to_lower(tipo_documento) %>% 
           rm_accent() %>% 
           str_squish()) %>% 
  mutate(categoria_de_tipo = case_when(
    tipo_documento %in% limpeza_de_tipos$op$cases ~ limpeza_de_tipos$op$convert,
    tipo_documento %in% limpeza_de_tipos$mn$cases ~ limpeza_de_tipos$mn$convert,
    tipo_documento %in% limpeza_de_tipos$ai$cases ~ limpeza_de_tipos$ai$convert,
    tipo_documento %in% limpeza_de_tipos$rp$cases ~ limpeza_de_tipos$rp$convert,
    tipo_documento %in% limpeza_de_tipos$tp$cases ~ limpeza_de_tipos$tp$convert,
    tipo_documento %in% limpeza_de_tipos$rai$cases ~ limpeza_de_tipos$rai$convert,
    tipo_documento %in% limpeza_de_tipos$rreq$cases ~ limpeza_de_tipos$rreq$convert,
    tipo_documento %in% limpeza_de_tipos$af$cases ~ limpeza_de_tipos$af$convert,
    tipo_documento %in% limpeza_de_tipos$am$cases ~ limpeza_de_tipos$am$convert,
    tipo_documento %in% limpeza_de_tipos$rarb$cases ~ limpeza_de_tipos$rarb$convert,
    tipo_documento %in% limpeza_de_tipos$ep$cases ~ limpeza_de_tipos$ep$convert,
    tipo_documento %in% limpeza_de_tipos$tarb$cases ~ limpeza_de_tipos$tarb$convert,
    tipo_documento %in% limpeza_de_tipos$sentarb$cases ~ limpeza_de_tipos$sentarb$convert,
    tipo_documento %in% limpeza_de_tipos$sentpar$cases ~ limpeza_de_tipos$sentpar$convert,
    tipo_documento %in% limpeza_de_tipos$addsent$cases ~ limpeza_de_tipos$addsent$convert,
    tipo_documento %in% limpeza_de_tipos$rreqarb$cases ~ limpeza_de_tipos$rreqarb$convert,
    tipo_documento %in% limpeza_de_tipos$psus$cases ~ limpeza_de_tipos$psus$convert,
    tipo_documento %in% limpeza_de_tipos$senthomolog$cases ~ limpeza_de_tipos$senthomolog$convert,
    tipo_documento %in% limpeza_de_tipos$addsentpar$cases ~ limpeza_de_tipos$addsentpar$convert,
    tipo_documento %in% limpeza_de_tipos$pedrec$cases ~ limpeza_de_tipos$pedrec$convert,
    tipo_documento %in% limpeza_de_tipos$comparb$cases ~ limpeza_de_tipos$comparb$convert,
    tipo_documento %in% limpeza_de_tipos$impugques$cases ~ limpeza_de_tipos$impugques$convert,
    tipo_documento %in% limpeza_de_tipos$indiqassques$cases ~ limpeza_de_tipos$indiqassques$convert,
    tipo_documento %in% limpeza_de_tipos$pedesc$cases ~ limpeza_de_tipos$pedesc$convert,
    tipo_documento %in% limpeza_de_tipos$resprec$cases ~ limpeza_de_tipos$resprec$convert,
    tipo_documento %in% limpeza_de_tipos$adita$cases ~ limpeza_de_tipos$adita$convert,
    tipo_documento %in% limpeza_de_tipos$pedcaut$cases ~ limpeza_de_tipos$pedcaut$convert,
    tipo_documento %in% limpeza_de_tipos$alegfinpar$cases ~ limpeza_de_tipos$alegfinpar$convert,
    tipo_documento %in% limpeza_de_tipos$ataud$cases ~ limpeza_de_tipos$ataud$convert,
    tipo_documento %in% limpeza_de_tipos$decaut$cases ~ limpeza_de_tipos$decaut$convert,
    tipo_documento %in% limpeza_de_tipos$decemerg$cases ~ limpeza_de_tipos$decemerg$convert,
    tipo_documento %in% limpeza_de_tipos$decext$cases ~ limpeza_de_tipos$decext$convert,
    tipo_documento %in% limpeza_de_tipos$decconfid$cases ~ limpeza_de_tipos$decconfid$convert,
    tipo_documento %in% limpeza_de_tipos$decurg$cases ~ limpeza_de_tipos$decurg$convert,
    tipo_documento %in% limpeza_de_tipos$impugpro$cases ~ limpeza_de_tipos$impugpro$convert,
    tipo_documento %in% limpeza_de_tipos$junt$cases ~ limpeza_de_tipos$junt$convert,
    tipo_documento %in% limpeza_de_tipos$notific$cases ~ limpeza_de_tipos$notific$convert,
    tipo_documento %in% limpeza_de_tipos$reqaobj$cases ~ limpeza_de_tipos$reqaobj$convert,
    tipo_documento %in% limpeza_de_tipos$reprevlim$cases ~ limpeza_de_tipos$reprevlim$convert,
    tipo_documento %in% limpeza_de_tipos$repimpug$cases ~ limpeza_de_tipos$repimpug$convert,
    tipo_documento %in% limpeza_de_tipos$respanp$cases ~ limpeza_de_tipos$respanp$convert,
    tipo_documento %in% limpeza_de_tipos$senthomologdesist$cases ~ limpeza_de_tipos$senthomologdesist$convert,
    tipo_documento %in% limpeza_de_tipos$sentexcuni$cases ~ limpeza_de_tipos$sentexcuni$convert,
    TRUE ~ "CASO ABERTO"
  ))

df_documentos %>% 
  filter(categoria_de_tipo == "CASO ABERTO")

## Remoção de documentos duplicados ----

teste_mesmo_doc_dup <- df_documentos %>% 
  mutate(teste_mesmo_doc = paste(categoria_de_tipo, id_arbitragem, tamanho_documento, sep = "_")) %>%
  filter(duplicated(teste_mesmo_doc)) %>% 
  .$teste_mesmo_doc

df_documentos %>% 
  mutate(teste_mesmo_doc = paste(categoria_de_tipo, id_arbitragem, tamanho_documento, sep = "_")) %>%
  filter(teste_mesmo_doc %in% teste_mesmo_doc_dup) %>% 
  arrange(teste_mesmo_doc) %>% 
  select(id_documento, teste_mesmo_doc, tamanho_documento)

df_documentos_not_dup <- df_documentos %>% 
  mutate(teste_mesmo_doc = paste(categoria_de_tipo, id_arbitragem, tamanho_documento, sep = "_")) %>%
  filter(!duplicated(teste_mesmo_doc))

## Análise documentos ----

### Documentos disponíveis, por tipo ----

df_documentos_not_dup %>%
  mutate(categoria_de_tipo = fct_infreq(categoria_de_tipo) %>% 
           fct_lump_min(10, other_level = "Outros Tipos"),
         fill_case = categoria_de_tipo == "Outros Tipos") %>%
  ggplot(aes(y = fct_rev(categoria_de_tipo), fill = fill_case)) +
  geom_bar() +
  labs(title = "Tipos de documentos mais frequentemente disponíveis",
       subtitle = subtitle,
       x = "Número de Documentos",
       y = "Tipo de Documento") +
  scale_fill_manual(values = c(single, "darkgray")) +
  theme(legend.position = "none")
  
save_plot("22_docs_disponiveis.png")

### Modo de acesso dos documentos ----

df_documentos_not_dup_count_tipos <- df_documentos_not_dup %>% 
  mutate(categoria_de_tipo = fct_lump_min(categoria_de_tipo, 10, other_level = "Outros Tipos")) %>% 
  count(categoria_de_tipo) %>% 
  mutate(categoria_de_tipo_count = paste0(
    categoria_de_tipo, " (N = ", n, ")"
  ))

df_documentos_not_dup %>%
  mutate(categoria_de_tipo = fct_lump_min(categoria_de_tipo, 10, other_level = "Outros Tipos")) %>% 
  left_join(df_documentos_not_dup_count_tipos, by = "categoria_de_tipo") %>% 
  mutate(
    is_site = (modo_de_acesso == "Site") %>% as.numeric(),
    categoria_de_tipo_count = categoria_de_tipo_count %>%
      fct_reorder(is_site, .fun = mean) %>%
      fct_relevel("Outros Tipos (N = 61)")
  ) %>%
  ggplot(aes(y = categoria_de_tipo_count, fill = modo_de_acesso)) +
  geom_bar(position = "fill") +
  labs(title = "Disponibilidade e Acesso aos Documentos",
       subtitle = subtitle,
       x = "Percentual dos Documentos",
       y = "Tipo de Documento",
       fill = "Acesso via:") +
  scale_fill_manual(values = pal(6)[c(5, 3, 1)]) +
  scale_x_continuous(labels = scales::label_percent()) +
  theme(legend.position = "right")

save_plot("23_acesso_aos_docs.png")

df_documentos_not_dup %>%
  mutate(categoria_de_tipo = fct_lump_min(categoria_de_tipo, 10, other_level = "Outros Tipos")) %>% 
  left_join(df_documentos_not_dup_count_tipos, by = "categoria_de_tipo") %>% 
  mutate(categoria_de_tipo_count = fct_reorder(categoria_de_tipo_count, tamanho_documento) %>% 
           fct_relevel("Outros Tipos (N = 61)"),
         color_case = categoria_de_tipo == "Outros Tipos") %>%
  ggplot(aes(y = categoria_de_tipo_count, x = tamanho_documento, fill = color_case, color = color_case)) +
  geom_boxplot() +
  labs(title = "Tamanho dos documentos, em número de páginas",
       subtitle = subtitle,
       x = "Número de Páginas dos Documentos",
       y = "Tipo de Documento") +
  scale_color_manual(values = c(single, "darkgray")) +
  scale_fill_manual(values = c(pal(10)[9], "gray95")) +
  theme(legend.position = "none")

save_plot("24_docs_tamanhos.png")

df_documentos_not_dup_info_arbitragens <- df_documentos_not_dup  %>%
  mutate(id_arbitragem = str_squish(id_arbitragem) %>% 
           str_replace("2O", "20")) %>% 
  left_join(select(
    df_arbitragens, id_arbitragem, uf_arbitragem, setor, camara, ano_inicio, data_fim, sucumbencia_prevista_para_pp
  ), by = "id_arbitragem") %>% 
  mutate(ano_fim = ifelse(data_fim %in% c("N/A", "N/D"), data_fim, str_extract(data_fim, "\\d{4}")) %>%
           factor(ordered = T, levels = c(as.character(2008:2024), "N/D", "N/A")))
df_documentos_not_dup_info_arbitragens %>%
  filter(!is.na(uf_arbitragem)) %>% 
  mutate(categoria_de_tipo = fct_lump_min(categoria_de_tipo, 10, other_level = "Outros Tipos")) %>%
  filter(!(ano_fim %in% c("N/A", "N/D"))) %>% 
  filter(categoria_de_tipo == "Sentença Arbitral Final") %>%
  mutate(ano_fim = ano_fim %>% as.character() %>% as.numeric()) %>% 
  ggplot(aes(x = ano_fim, y = tamanho_documento, fill = uf_arbitragem)) +
  geom_point(alpha = 0.8, size = 3, color = "black", shape = 21) +
  scale_x_continuous(breaks = seq(2008, 2024, by = 4)) +
  labs(title = "A Sentença Arbitral Final está se tornando mais longa nos últimos anos",
       subtitle = subtitle,
       x = "Ano de Término da Arbitragem",
       y = "Tamanho da Sentença Arbitral Final",
       fill = "Unidade Federativa: ") +
  scale_fill_manual(values = pal(7)[c(1,3,5)])

save_plot("25_sentenca_mais_longa_tempo.png")

# CITAÇÕES EM DOCUMENTOS ----

df_citacoes <- read_xlsx("2024_08_04/input/df.xlsx", sheet = "citacoes_em_documentos", col_types = rep("text", 4)) %>% 
  janitor::clean_names()

pal_green <- colorspace::choose_palette()

df_citacoes_documentos <- df_citacoes %>% 
  mutate(id_documento = case_when(
    id_documento == "35.4" ~ "35.04",
    id_documento == "8.1" ~ "8.1.",
    TRUE ~ id_documento
  )) %>% 
  left_join(df_documentos, by = "id_documento") %>% 
  filter(id_arbitragem != "56")

df_citacoes_documentos %>% 
  mutate(autor_mais_citado = fct_lump_n(autor_citado, 10)) %>%
  filter(autor_mais_citado != "Other") %>%
  filter(str_detect(str_to_lower(categoria_de_tipo), "senten[çc]a")) %>% 
  ggplot(aes(y = autor_mais_citado %>% fct_infreq() %>% fct_rev())) +
  geom_bar(fill=single) +
  labs(title = "Os 10 autores mais citados nas sentenças das arbitragens",
       subtitle = subtitle,
       y = "Autor", x = "Número de Citações") 

save_plot("26_autores_mais_citados.png")

df_autor_mais_citado_n <- df_citacoes_documentos %>%
  filter(str_detect(str_to_lower(categoria_de_tipo), "senten[çc]a")) %>% 
  mutate(autor_mais_citado = fct_lump_n(autor_citado, 10)) %>%
  filter(autor_mais_citado != "Other") %>%
  count(autor_mais_citado) %>% 
  mutate(autor_mais_citado_n = paste0(autor_mais_citado, " (N = ", n, ")")) %>% 
  select(!n)

df_citacoes_documentos %>% 
  filter(str_detect(str_to_lower(categoria_de_tipo), "senten[çc]a")) %>% 
  mutate(autor_mais_citado = fct_lump_n(autor_citado, 10)) %>%
  filter(autor_mais_citado != "Other") %>%
  left_join(df_arbitragens, by = "id_arbitragem") %>% 
  left_join(df_autor_mais_citado_n, by = "autor_mais_citado") %>% 
  select(autor_mais_citado, autor_mais_citado_n, uf_arbitragem) %>% 
  mutate(is_federal = (uf_arbitragem == "Federal") %>% as.numeric(),
         autor_mais_citado_n = fct_reorder(autor_mais_citado_n, is_federal, .fun = mean)) %>%
  ggplot(aes(y = autor_mais_citado_n, fill = fct_rev(uf_arbitragem))) +
  geom_bar(position = "fill") +
  labs(title = "Autores mais citados: proporções em arbitragens federais e estaduais",
       subtitle = subtitle,
       y = "Autor", x = "Percentual das Citações", fill = "Unidade Federativa: ") +
  scale_fill_manual(values = pal(4)[1:3] %>% rev(), guide = guide_legend(reverse = TRUE)) +
  scale_x_continuous(labels = scales::label_percent())

save_plot("27_autores_mais_citados_participacao_das_ufs.png")

## Extra ----

# df_citacoes_documentos %>% 
#   mutate(autor_citado = str_split(autor_citado, ";")) %>% 
#   select(autor_citado) %>% 
#   unnest(autor_citado) %>% 
#   count(autor_citado, sort = TRUE) %>%
#   export_table("x_autores_citados_nas_arbitragens.csv")

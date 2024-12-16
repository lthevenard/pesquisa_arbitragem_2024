library(tidyverse)
library(lubridate)
library(readxl)

# Functions ----

save_plot <- function(filename, path = "./2024_12_16/output/plots", height = 6, width = 10, dpi = 300, ...) {
  ggsave(filename=filename, height=height, width=width, path=path, dpi=dpi, ...)
}

export_table <- function(df, filename, folder = "./2024_12_16/output/tabelas/") {
  df %>%
    write_excel_csv2(paste0(folder, filename))
}

perc_to_text <- function(perc, acc = 0.1) {
  scales::label_percent(accuracy=acc, decimal.mark=",")(perc)
}

perc_and_n_to_text <- function(n, perc, acc = 0.1) {
  paste0(perc_to_text(perc, acc), " (N = ", n, ")")
}

n_and_perc_to_text <- function(n, perc, acc = 0.1) {
  paste0(n, " (", perc_to_text(perc, acc), ")")
}

count_and_perc <- function(grouped_df, acc=0.1) {
  grouped_df %>% 
    summarise(n = n()) %>% 
    mutate(perc = n / sum(n),
           lbl_perc = perc_to_text(perc, acc),
           lbl_perc_n = perc_and_n_to_text(n, perc, acc),
           lbl_n_perc = n_and_perc_to_text(n, perc, acc))
}


export_table <- function(df, filename, folder = "./2024_12_16/output/dados/") {
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

source("R/plotlib/plotlib.R")

## Dados ----
### Arbitragens ----


df_arbitragens <- read_xlsx("2024_12_16/input/arbitragem.xlsx", sheet = "arbitragens", col_types = rep("text", 33)) %>% 
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
    resultado_final = fct_infreq(resultado_final) %>% fct_relevel("N/A", after = Inf) %>% fct_relevel("N/D", after = Inf),
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

df_arbitragens <- df_arbitragens %>%
  mutate(dias_ate_fim = map2_dbl(data_inicio, data_fim, calc_date_dist))

n_arbitragens <- df_arbitragens %>% nrow()

df_arbitragens_objetos <- df_arbitragens %>% 
  mutate(objeto = str_split(objeto, "\\s?;\\s?")) %>% 
  select(id_arbitragem, resultado_final, objeto, dias_ate_fim) %>% 
  unnest(objeto) %>%
  mutate(objeto=str_to_title(objeto) %>% str_replace("De", "de")) %>% 
  filter(objeto!="N/D")

honorarios_sucumbencia <- df_arbitragens %>%
  select(id_arbitragem, valor_honrarios_sucumbencia) %>% 
  mutate(honorarios_sucumbencia = str_replace_all(valor_honrarios_sucumbencia, 
                                                  "(?<=\\d),(?=\\d)", ".") %>%
           str_extract_all("[\\d\\.]+")) %>% 
  unnest(honorarios_sucumbencia) %>% 
  mutate(honorarios_sucumbencia=as.numeric(honorarios_sucumbencia)) %>% 
  group_by(id_arbitragem) %>% 
  summarise(total_honorarios_sucumbencia=sum(honorarios_sucumbencia))

honorarios_arbitros <- df_arbitragens %>%
  select(id_arbitragem, valor_dos_honorarios) %>% 
  filter(!str_detect(valor_dos_honorarios, "hora")) %>% 
  mutate(honorarios_arbitros = str_replace_all(valor_dos_honorarios,
                                               "(?<=\\d),(?=\\d)", ".") %>%
           str_extract_all("[\\d\\.]+(?!\\))")) %>%
  unnest(honorarios_arbitros) %>%
  mutate(honorarios_arbitros=as.numeric(honorarios_arbitros)) %>%
  group_by(id_arbitragem) %>% 
  summarise(total_honorarios_arbitros=sum(honorarios_arbitros))

honorarios_arbitros_discriminados <- df_arbitragens %>%
  select(id_arbitragem, valor_dos_honorarios) %>% 
  filter(!str_detect(valor_dos_honorarios, "hora") & str_detect(valor_dos_honorarios, ";")) %>%
  mutate(honorarios_split=str_split(valor_dos_honorarios, ";")) %>% 
  unnest(honorarios_split) %>%
  mutate(honorarios_split=str_squish(honorarios_split)) %>% 
  filter(honorarios_split!="") %>%
  mutate(
    valor_honorarios_discriminados=str_replace_all(
      honorarios_split,
      "(?<=\\d),(?=\\d)", ".") %>%
      str_extract_all("[\\d\\.]+(?!\\))") %>% 
      as.numeric(),
    atribuicao_honorarios_discriminados=str_extract(
      honorarios_split,
      "(?<=\\().+(?=\\))"
    ) %>% 
      str_remove("\\d+") %>% 
      str_squish() %>% 
      str_to_title()
  ) %>% 
  filter(atribuicao_honorarios_discriminados!="Árbitros")

##  Plots ----

{theme_bw() + theme(legend.position = "bottom")}

theme_set({{theme_bw() + theme(legend.position = "bottom")}})

pal <- colorspace::choose_palette()
single <- pal(5)[2]
pal_uf <- pal(4)

subtitle <- "Pesquisa de arbitragens (2002-2024)"
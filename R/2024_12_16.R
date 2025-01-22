library(tidyverse)
library(lubridate)
library(readxl)

# Functions ----

save_plot <- function(filename, path = "./output/2024_12_16/graficos", height = 6, width = 10, dpi = 300, ...) {
  ggsave(filename=filename, height=height, width=width, path=path, dpi=dpi, ...)
}

export_table <- function(df, filename, folder = "./output/2024_12_16/tabelas/") {
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

calc_date_dist <- function(date_begin, date_end) {
  if (date_end %in% c("N/A", "N/D") | date_begin %in% c("N/A", "N/D")) {
    return(NA)
  } else {
    return(
      as.numeric(ymd(date_end)) - as.numeric(ymd(date_begin))
    )
  }
}

evolucao_temporal_ajuste <- function(p) {
  p +
    geom_vline(xintercept = "", size = 0.25) +
    scale_x_discrete(drop = FALSE, expand = expansion(mult = c(0.045, 0.045))) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
          legend.position = "bottom",
          axis.ticks.x = element_line(colour =c(rep("black", 22), "transparent", "black")))
}

source("R/plotlib/plotlib.R")

## Dados ----
### Arbitragens ----


df_arbitragens <- read_xlsx("./input/2024_12_16_arbitragem.xlsx", sheet = "arbitragens", col_types = rep("text", 33)) %>% 
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
    )) %>%
  select(!x33)

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

theme_set({{theme_bw() + theme(legend.position = "bottom")}})

pal <- readRDS("input/pal_function.rds")
single <- pal(5)[2]
pal_uf <- pal(4)

subtitle <- "Pesquisa de arbitragens (01/2002 a 06/2024)"

### 1 - Transparência geral ----

categorias_transparencia <- c(
  "Sem transparência",
  "Apenas transparência passiva",
  "Há pouca transparência ativa",
  "Há muita transparência ativa"
)

df_arbitragens <- df_arbitragens %>%
  mutate(transparencia = ifelse(
    transparencia == "Há transparência ativa",
    categorias_transparencia[[3]],
    transparencia
  ) %>% 
    str_remove("\\.") %>% 
    str_replace("N/A", categorias_transparencia[[1]]) %>%
    factor(ordered=T, levels=categorias_transparencia))

df_arbitragens %>%
  group_by(transparencia) %>% 
  count_and_perc() %>%
  ggplot(aes(x=transparencia, y=n, label=lbl_n_perc)) +
  geom_col(fill=single) +
  geom_text(color=single, fontface="bold", nudge_y = 0.5, size=2.8) +
  labs(title="Grau de transparência das arbitragens",
       subtitle=subtitle,
       x="Grau de transparência", y="Número de arbitragens")

save_plot("1_transparencia.png")

### 2 - Transparência por UF ----

df_arbitragens %>%
  mutate(uf_arbitragem = ifelse(uf_arbitragem == "Federal", "Federais", "Estaduais (SP e RJ)") %>% fct_rev()) %>% 
  group_by(uf_arbitragem, transparencia) %>% 
  count_and_perc() %>%
  ggplot(aes(y=fct_rev(transparencia), x=n, label=lbl_n_perc,
             color=uf_arbitragem, fill=uf_arbitragem)) +
  geom_col() +
  geom_text(fontface="bold", nudge_x = 3, size=2.8) +
  scale_color_manual(values=pal_uf) +
  scale_fill_manual(values=pal_uf) +
  labs(title="Grau de transparência das arbitragens",
       subtitle=subtitle,
       y="Grau de transparência",
       x="Número de arbitragens") +
  facet_grid(~uf_arbitragem) +
  scale_x_continuous(limits=c(0, 33)) +
  theme(legend.position="none")

save_plot("2_transparencia_por_uf.png")

### 3 - Arbitragens por ano ----

df_arbitragens %>%
  mutate(ano_inicio = ifelse(data_inicio == "N/D", "N/D", str_extract(data_inicio, "\\d{4}")) %>%
           factor(ordered = T, levels = c(as.character(2002:2023), "", "N/D"))) %>%
  group_by(ano_inicio, uf_arbitragem) %>% 
  count_and_perc() %>%
  mutate(lbl_n = as.character(n)) %>% 
  plot_fillcol(
    "ano_inicio",
    "uf_arbitragem",
    title_label="Arbitragens ao longo do tempo, por Unidade Federativa",
    x_label="Ano de início da arbitragem",
    y_label="Número de arbitragens identificadas",
    fill_label="Unidade Federativa: ",
    fill_position="stack",
    order_bars=FALSE,
    fill_colors=pal_uf,
    var_label="lbl_n",
    lbl_colors="white",
    theme_func=theme_bw
  ) %>%
  evolucao_temporal_ajuste()

save_plot("3_evolucao_anual_stack.png")

### 4 - Tempo do problema ao início da arbitragem por ano ----

df_arbitragens %>%
  mutate(dias_ate_inicio_desde_problema = map2_dbl(data_problema, data_inicio, calc_date_dist)) %>%
  filter(!is.na(dias_ate_inicio_desde_problema)) %>%
  mutate(ano_inicio = str_extract(data_inicio, "\\d{4}") %>% as.numeric()) %>%
  ggplot(aes(x=ano_inicio, y=dias_ate_inicio_desde_problema)) +
  scale_x_continuous(breaks=2006:2022) +
  geom_smooth(method="lm", se=F, color="pink", linetype="dashed") +
  geom_point(color=single, alpha=0.6, size=3) +
  stat_summary(geom="pointrange", fun= function(x) {ifelse(length(x) < 2, NA, mean(x))}, color=pal_uf[1], shape=8) +
  labs(title="Tempo desde a ocorrência do problema até o início da arbitragem, por ano da arbitragem",
       subtitle = subtitle,
       x = "Ano de início da arbitragem",
       y = "Tempo, em dias, desde a ocorrência do problema até o início da arbitragem")

save_plot("4_tempo_problema_por_ano_scatter.png")

df_arbitragens %>%
  mutate(dias_ate_inicio_desde_problema = map2_dbl(data_problema, data_inicio, calc_date_dist)) %>%
  filter(!is.na(dias_ate_inicio_desde_problema)) %>%
  mutate(ano_inicio = str_extract(data_inicio, "\\d{4}") %>% as.numeric()) %>%
  group_by(ano_inicio) %>%
  summarise(n=n(), media=mean(dias_ate_inicio_desde_problema))

### 5 - Tempo do problema ao início da arbitragem por UF ----

uf_n_2l <- df_arbitragens %>%
  mutate(dias_ate_inicio_desde_problema = map2_dbl(data_problema, data_inicio, calc_date_dist)) %>%
  filter(!is.na(dias_ate_inicio_desde_problema)) %>%
  count(uf_arbitragem) %>%
  mutate(uf_n = paste0(uf_arbitragem, "\n(N = ", n, ")")) %>%
  select(!n)

df_arbitragens %>%
  mutate(dias_ate_inicio_desde_problema = map2_dbl(data_problema, data_inicio, calc_date_dist)) %>%
  filter(!is.na(dias_ate_inicio_desde_problema)) %>%
  left_join(uf_n_2l, by="uf_arbitragem") %>%
  ggplot(aes(x=fct_infreq(uf_n), y=dias_ate_inicio_desde_problema)) +
  geom_boxplot(color=single, fill=pal_uf[3]) +
  labs(title="Tempo desde a ocorrência do problema até o início da arbitragem, por unidade federativa",
       subtitle = subtitle,
       x = "Unidade Federativa",
       y = "Tempo, em dias, desde a ocorrência do problema até o início da arbitragem")

save_plot("5_tempo_problema_por_uf.png")

library(tidyverse)
library(lubridate)
library(readxl)

# Functions ----

source("R/aux_funcs.R")
source("R/plotlib/plotlib.R")

# Dados ----
## Arbitragens ----


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
  select(id_arbitragem, resultado_final, objeto, data_inicio, data_fim) %>% 
  unnest(objeto) %>%
  filter(objeto != "N/D") %>%
  mutate(objeto=paste("Se há", str_to_lower(objeto)))

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

df_atores <- read_xlsx("input/2024_12_16_atores.xlsx") %>%
  mutate(id_ator=as.character(id_ator))

df_relacao_ator_arbitragem <- read_xlsx("input/2024_12_16_arbitragem.xlsx", sheet="relacao_ator_arbitragem") %>% 
  mutate(id_arbitragem = as.character(id_arbitragem),
         id_ator=as.character(id_ator)) %>% 
  left_join(df_arbitragens, by="id_arbitragem") %>% 
  left_join(df_atores, by="id_ator")



## Documentos ----

df_documentos <- read_xlsx("input/2024_12_16_arbitragem.xlsx", sheet = "documentos", col_types = c(rep("text", 4), "numeric")) %>% 
  janitor::clean_names()

df_documentos <- df_documentos %>%
  mutate(macro_tipo = case_when(
    str_detect(tipo_documento, "Sentença") ~ "Sentença",
    str_detect(tipo_documento, "finais") ~ "Alegações Finais",
    str_detect(tipo_documento, "Requerimento") ~ "Requerimento de Arbitragem",
    
    TRUE ~ tipo_documento
  ))

df_documentos_expandida <- df_documentos %>%
  bind_rows(
    read_csv("input/documentos_aux.csv") %>%
      mutate(id_documento = as.character(id_documento),
             id_arbitragem = as.character(id_arbitragem))
  )

#  Plots ----

theme_set({{theme_bw() + theme(legend.position = "bottom")}})

pal <- readRDS("input/pal_function.rds")
single <- pal(5)[2]
pal_uf <- pal(4)

subtitle <- "Pesquisa de arbitragens (01/2002 a 06/2024)"

## Transparência e Número de arbitragens ----
### 1 - Transparência geral

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

### 2 - Transparência por UF

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
  labs(title="Grau de transparência das arbitragens por UF",
       subtitle=subtitle,
       y="Grau de transparência",
       x="Número de arbitragens") +
  facet_grid(~uf_arbitragem) +
  scale_x_continuous(limits=c(0, 33)) +
  theme(legend.position="none")

save_plot("2_transparencia_por_uf.png")

### 3 - Arbitragens por ano

df_arbitragens %>%
  mutate(ano_inicio = ifelse(data_inicio == "N/D", "N/D", str_extract(data_inicio, "\\d{4}")) %>%
           factor(ordered = T, levels = c(as.character(2002:2023), "", "N/D"))) %>%
  group_by(ano_inicio, uf_arbitragem) %>% 
  count_and_perc() %>%
  mutate(lbl_n = as.character(n)) %>% 
  plot_fillcol(
    "ano_inicio",
    "uf_arbitragem",
    title_label="Arbitragens ao longo do tempo, por UF",
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

## Tempo do problema ao início da arbitragem ----
### 4 - Tempo do problema ao início da arbitragem por ano

df_arbitragens %>%
  time_scatter_by_year(
    begin_col="data_problema",
    end_col="data_inicio",
    first_year=2006,
    last_year=2022,
    time_description="entre o evento litigioso e o início da arbitragem"
  )

save_plot("4_tempo_problema_por_ano.png")

df_arbitragens %>%
  time_n_mean_per_year(
    begin_col="data_problema",
    end_col="data_inicio"
  )


### 5 - Tempo do problema ao início da arbitragem por UF

df_arbitragens %>%
  time_boxplot_by_var(
    var_col="uf_arbitragem",
    begin_col="data_problema",
    end_col="data_inicio",
    var_label="Unidade Federativa",
    time_description="entre o evento litigioso e o início da arbitragem",
    var_description="UF",
    n_lines=2
  )

save_plot("5_tempo_problema_por_uf.png")

df_arbitragens %>%
  time_prepare_interval_df("data_problema", "data_inicio") %>%
  from_df_to_summary("uf_arbitragem", "time_interval")
  

### 6 - Tempo do problema ao início da arbitragem por menção à perícia

df_arbitragens %>%
  time_boxplot_by_var_reordering(
    var_col="mencao_de_pericia_na_sentenca",
    begin_col="data_problema",
    end_col="data_inicio",
    var_label="Houve menção à realização de perícia na sentença?",
    time_description="entre o evento litigioso e o início da arbitragem",
    var_description="menção a perícia",
    reordering_var_n_func=time_reorder_var_pericia,
    n_lines=2
  )

save_plot("6_tempo_problema_por_mencao_pericia.png")

df_arbitragens %>%
  time_prepare_interval_df("data_problema", "data_inicio") %>%
  from_df_to_summary("mencao_de_pericia_na_sentenca", "time_interval")


## Tempo do contrato ao início da arbitragem ----
### 7 - Tempo do contrato ao início da arbitragem por ano

df_arbitragens %>%
  time_scatter_by_year(
    begin_col="inicio_contrato",
    end_col="data_inicio",
    first_year=2002,
    last_year=2022,
    time_description="entre o início do contrato e o início da arbitragem"
  )

save_plot("7_tempo_contrato_por_ano.png")

df_arbitragens %>%
  time_n_mean_per_year(
    begin_col="data_problema",
    end_col="data_inicio"
  )

### 8 - Tempo do contrato ao início da arbitragem por UF

df_arbitragens %>%
  time_boxplot_by_var(
    var_col="uf_arbitragem",
    begin_col="inicio_contrato",
    end_col="data_inicio",
    var_label="Unidade Federativa",
    time_description="entre o início do contrato e o início da arbitragem",
    var_description="UF",
    n_lines=2
  )

save_plot("8_tempo_contrato_por_uf.png")

df_arbitragens %>%
  time_prepare_interval_df("inicio_contrato", "data_inicio") %>%
  from_df_to_summary("uf_arbitragem", "time_interval")


### 9 - Tempo do contrato ao início da arbitragem por menção à perícia 

df_arbitragens %>%
  time_boxplot_by_var_reordering(
    var_col="mencao_de_pericia_na_sentenca",
    begin_col="inicio_contrato",
    end_col="data_inicio",
    var_label="Houve menção à realização de perícia na sentença?",
    time_description="entre o início do contrato e o início da arbitragem",
    var_description="menção a perícia",
    reordering_var_n_func=time_reorder_var_pericia,
    n_lines=2
  )

save_plot("9_tempo_contrato_por_mencao_pericia.png")

df_arbitragens %>%
  time_prepare_interval_df("inicio_contrato", "data_inicio") %>%
  from_df_to_summary("mencao_de_pericia_na_sentenca", "time_interval")

## Tempo do requerimento da arbitragem à assinatura do termo ----

### 10 - Tempo do requerimento da arbitragem à assinatura do termo por ano 

df_arbitragens %>%
  time_scatter_by_year(
    begin_col="data_inicio",
    end_col="data_termo",
    first_year=2002,
    last_year=2023,
    time_description="entre o requerimento da arbitragem e a assinatura do termo"
  )

save_plot("10_tempo_termo_por_ano.png")

df_arbitragens %>%
  time_n_mean_per_year(
    begin_col="data_inicio",
    end_col="data_termo"
  )

### 11 - Tempo do requerimento da arbitragem à assinatura do termo por UF

df_arbitragens %>%
  time_boxplot_by_var(
    var_col="uf_arbitragem",
    begin_col="data_inicio",
    end_col="data_termo",
    var_label="Unidade Federativa",
    time_description="entre o requerimento da arbitragem e a assinatura do termo",
    var_description="UF",
    n_lines=2
  ) +
  scale_y_continuous(limits=c(0, 1000))

save_plot("11_tempo_termo_por_uf.png")

df_arbitragens %>%
  time_prepare_interval_df("data_inicio", "data_termo") %>%
  from_df_to_summary("uf_arbitragem", "time_interval")


### 12 - Tempo do requerimento da arbitragem à assinatura do termo por menção à perícia

df_arbitragens %>%
  time_boxplot_by_var_reordering(
    var_col="mencao_de_pericia_na_sentenca",
    begin_col="data_inicio",
    end_col="data_termo",
    var_label="Houve menção à realização de perícia na sentença?",
    time_description="entre o requerimento da arbitragem e a assinatura do termo",
    var_description="menção a perícia",
    reordering_var_n_func=time_reorder_var_pericia,
    n_lines=2
  ) +
  scale_y_continuous(limits=c(0, 1000))

save_plot("12_tempo_termo_por_mencao_pericia.png")

df_arbitragens %>%
  time_prepare_interval_df("data_inicio", "data_termo") %>%
  from_df_to_summary("mencao_de_pericia_na_sentenca", "time_interval")

## Tempo de duração da arbitragem ----

### 13 - Tempo de duração da arbitragem por ano

df_arbitragens %>%
  time_scatter_by_year(
    begin_col="data_inicio",
    end_col="data_fim",
    first_year=2002,
    time_description="do início ao fim da arbitragem"
  )

save_plot("13a_tempo_duracao_por_ano.png")

df_arbitragens %>%
  time_n_mean_per_year(
    begin_col="data_inicio",
    end_col="data_fim"
  )

df_arbitragens %>%
  time_scatter_by_year_of_end(
    begin_col="data_inicio",
    end_col="data_fim",
    first_year=2008,
    last_year=2024,
    time_description="do início ao fim da arbitragem"
  )

save_plot("13b_tempo_duracao_por_ano_fim.png")

df_arbitragens %>%
  time_n_mean_per_year(
    begin_col="data_inicio",
    end_col="data_fim"
  )

### 14 - Tempo de duração da arbitragem por UF

df_arbitragens %>%
  time_boxplot_by_var(
    var_col="uf_arbitragem",
    begin_col="data_inicio",
    end_col="data_fim",
    var_label="Unidade Federativa",
    time_description="do início ao fim da arbitragem",
    var_description="UF",
    n_lines=2
  ) +
  scale_y_continuous(limits=c(0, 2500))

save_plot("14_tempo_duracao_por_uf.png")

df_arbitragens %>%
  time_prepare_interval_df("data_inicio", "data_fim") %>%
  from_df_to_summary("uf_arbitragem", "time_interval")


### 15 - Tempo de duração da arbitragem por menção à perícia

df_arbitragens %>%
  time_boxplot_by_var_reordering(
    var_col="mencao_de_pericia_na_sentenca",
    begin_col="data_inicio",
    end_col="data_fim",
    var_label="Houve menção à realização de perícia na sentença?",
    time_description="do início ao fim da arbitragem",
    var_description="menção a perícia",
    reordering_var_n_func=time_reorder_var_pericia,
    n_lines=2
  ) +
  scale_y_continuous(limits=c(0, 2500))

save_plot("15_tempo_duracao_por_mencao_pericia.png")

df_arbitragens %>%
  time_prepare_interval_df("data_inicio", "data_termo") %>%
  from_df_to_summary("mencao_de_pericia_na_sentenca", "time_interval")

### 16 - Tempo de duração da arbitragem por resultado final

df_arbitragens_resultados <- df_arbitragens %>%
  mutate(resultado_final  = ifelse(str_detect(resultado_final, "pedido"), "Improcedente", as.character(resultado_final))) %>%
  filter(resultado_final != "N/A")

df_arbitragens_resultados %>%
  time_boxplot_by_var(
    var_col="resultado_final",
    begin_col="data_inicio",
    end_col="data_fim",
    var_label="Resultado Final",
    time_description="do início ao fim da arbitragem",
    var_description="resultado final",
    n_lines=1
  ) +
  scale_y_continuous(limits=c(0, 2500)) +
  coord_flip()

save_plot("16_tempo_duracao_por_resultado.png")

df_arbitragens_resultados %>%
  time_prepare_interval_df("data_inicio", "data_termo") %>%
  from_df_to_summary("resultado_final", "time_interval")

### 17 - Tempo de duração da arbitragem por acordo

df_arbitragens_resultados %>%
  mutate(resultado_acordo = ifelse(
    str_detect(resultado_final, "Acordo"),
    "Acordo",
    "Outros Resultados"
  )) %>%
  time_boxplot_by_var(
    var_col="resultado_acordo",
    begin_col="data_inicio",
    end_col="data_fim",
    var_label="Resultado Final",
    time_description="do início ao fim da arbitragem",
    var_description="resultado: Acordo vs. Outros Resultados",
    n_lines=1
  ) +
  scale_y_continuous(limits=c(0, 2500)) +
  coord_flip()

save_plot("17_tempo_duracao_por_acordo.png")

df_arbitragens_resultados %>%
  mutate(resultado_acordo = ifelse(
    str_detect(resultado_final, "Acordo"),
    "Acordo",
    "Outros Resultados",
  )) %>%
  time_prepare_interval_df("data_inicio", "data_termo") %>%
  from_df_to_summary("resultado_acordo", "time_interval")

### 18 - Tempo de duração da arbitragem por objeto

df_arbitragens_objetos %>%
  time_boxplot_by_var(
    var_col="objeto",
    begin_col="data_inicio",
    end_col="data_fim",
    var_label="Objeto",
    time_description="do início ao fim da arbitragem",
    var_description="objeto",
    n_lines=1
  ) +
  scale_y_continuous(limits=c(0, 2500)) +
  coord_flip()

save_plot("18_tempo_duracao_por_objeto.png")

## Setores ----

### 19 - Setores das arbitragens

df_arbitragens_setores <- df_arbitragens %>%
  mutate(setor = case_when(
    setor == "Energia Elétrica" ~ "Energia",
    setor == "N/A" | str_detect(setor, "PPP") ~ "N/A e Outros",
    TRUE ~ setor
  ) %>%
    fct_infreq() %>%
    fct_relevel("N/A e Outros", after=Inf) %>% 
    fct_rev())

df_arbitragens_setores %>%
  group_by(setor) %>%
  count_and_perc() %>%
  mutate(lbl_pos = case_when(
    n < 6 ~ n + 0.55,
    n < 10 ~ n + 0.65,
    TRUE ~ n + 0.7
  )) %>% 
  ggplot(aes(y=setor, x = n, label=lbl_n_perc)) +
  geom_col(fill=single) +
  geom_text(
    aes(x=lbl_pos), color=single, size=3, fontface="bold"
  ) +
  labs(
    title="Setores das arbitragens",
    subtitle=subtitle,
    x="Número de Arbitragens",
    y=""
  )

save_plot("19_setores.png")

### 20 - Setores das arbitragens por UF

df_arbitragens_setores %>%
  group_by(setor, uf_arbitragem) %>% 
  count_and_perc() %>%
  mutate(lbl_n = as.character(n)) %>% 
  plot_fillcol(
    "setor",
    "uf_arbitragem",
    title_label="Setores das arbitragens, por UF",
    x_label="Setor da arbitragem",
    y_label="Número de arbitragens identificadas",
    fill_label="Unidade Federativa: ",
    fill_position="stack",
    flip=TRUE,
    order_bars=FALSE,
    fill_colors=pal_uf,
    var_label="lbl_n",
    lbl_colors="white",
    theme_func=theme_bw
  ) +
  theme(legend.position="bottom")

save_plot("20_setores_por_uf.png")

## Árbitros ----

### 21 - Quem são os árbitros

df_relacao_ator_arbitragem %>%
  filter(str_detect(tipo_relacao, "Árbitro")) %>%
  group_by(nome) %>% 
  count_and_perc() %>%
  filter(n>4) %>%
  mutate(lbl_n = as.character(n),
         nome = fct_reorder(nome, n)) %>%
  ggplot(aes(y=nome, x = n)) +
  geom_col(fill=single) +
  labs(
    title="Árbitros mais indicados para participar das arbitragens",
    subtitle=subtitle,
    x="Número de Arbitragens",
    y=""
  ) +
  scale_x_continuous(breaks=seq(0, 8, 2))

save_plot("21_arbitros.png")

### 22 - Quem indicou os árbitros

arbitros_mais_indicados <- df_relacao_ator_arbitragem %>%
  filter(str_detect(tipo_relacao, "Árbitro")) %>%
  group_by(nome) %>% 
  count_and_perc() %>%
  filter(n>4) %>%
  .$nome

df_relacao_ator_arbitragem %>%
  filter(str_detect(tipo_relacao, "Árbitro")) %>%
  filter(nome %in% arbitros_mais_indicados) %>%
  mutate(
    quem_indicou = case_when(
      str_detect(quem_indicou, "ANP") ~ "Agência Reguladora",
      str_detect(quem_indicou, "Presidente") ~ "Coárbitros ou Pres. da Corte",
      str_detect(quem_indicou, "São Paulo") ~ "União, SP ou RJ",
      str_detect(quem_indicou, "Rio de Janeiro") ~ "União, SP ou RJ",
      str_detect(quem_indicou, "União") ~ "União, SP ou RJ",
      str_detect(quem_indicou, "CAF") ~ "Concessionária",
      str_detect(quem_indicou, "Concessionária") ~ "Concessionária",
      str_detect(quem_indicou, "Coarbitros") ~ "Coárbitros ou Pres. da Corte",
      str_detect(quem_indicou, "Complexo") ~ "Concessionária",
      str_detect(quem_indicou, "Requerente") ~ "Concessionária",
      TRUE ~ quem_indicou
    ),
    nome = fct_infreq(nome)) %>% 
  ggplot(aes(y=fct_infreq(quem_indicou) %>% fct_rev())) +
  geom_bar(fill=single) +
  labs(
    title="Quem indicou os árbitros mais vezes indicados?",
    subtitle=subtitle,
    y="Quem indicou",
    x="Número de indicações"
  ) +
  facet_wrap(~nome, ncol=3)

save_plot("22_quem_indicou_arbitros.png")

### 23 - Idade e gênero dos árbitros

df_idades_arbitros <- read_xlsx("input/2024_12_16_idades_arbitros.xlsx")

colunas_idade <- names(df_idades_arbitros)[2:7]

df_idades_arbitros <- df_idades_arbitros %>%
  mutate(id_ator=as.character(`Árbitros`)) %>% 
  pivot_longer(cols=colunas_idade, names_to="col", values_to="idade") %>%
  select(id_ator, idade) %>%
  filter(!is.na(idade))

df_idades_generos_arbitros <- df_idades_arbitros %>%
  left_join(select(df_atores, !idade), by="id_ator") %>%
  filter(!is.na(sexo)) %>%
  mutate(sexo = str_to_sentence(sexo) %>%
           str_replace("Mac", "Masc"))

df_idades_generos_arbitros %>%
  ggplot(aes(x=idade, fill=sexo)) +
  geom_histogram(bins=10, color="black") +
  labs(
    title="Idade e gênero dos árbitros",
    subtitle=subtitle,
    x="Idade (Anos)",
    y="Número de Árbitros",
    fill="Gênero dos Árbitros: "
  ) +
  scale_fill_manual(values=c(pal_uf[c(3, 1)], "Gray")) +
  theme(legend.position="bottom")

save_plot("23_idade_genero_arbitros.png")

### 24 - Comparações das idades por gênero

df_generos_arbitros_n <- df_idades_generos_arbitros %>%
  count(sexo) %>% 
  mutate(sexo_n = paste0(sexo, "\n(N = ", n, ")")) %>%
  select(!n)

df_idades_generos_arbitros %>%
  left_join(df_generos_arbitros_n, by="sexo") %>%
  ggplot(aes(x=sexo_n, y=idade)) +
  geom_boxplot(fill=pal_uf[3], color=single) +
  labs(title="Comparação das distribuições de idades dos árbitros por gênero",
       subtitle=subtitle,
       x= "Gênero dos Árbitros",
       y="Idade (Anos)")

save_plot("24_idade_genero_comparacao.png")

## Outros atores ----

### 25 - Impugnações por ator

df_relacao_ator_arbitragem %>%
  filter(tipo_relacao != "Advogada") %>%
  filter(houve_impugnacao == "Sim") %>%
  filter(!is.na(nome)) %>%
  mutate(
    tipo_num = case_when(
      tipo_relacao == "Árbitro" ~ 3,
      tipo_relacao == "Perito" ~ 2,
      tipo_relacao == "Parecerista" ~ 1
    ),
    nome = ifelse(str_detect(nome, "FLORIANO"), "Floriano de Azevedo Marques Neto", nome) %>%
      fct_reorder(tipo_num) %>%
      fct_relevel("José Vicente Santos Mendonça", after=Inf),
    tipo_relacao = fct_reorder(tipo_relacao, tipo_num) %>% fct_rev()
  ) %>%
  ggplot(aes(y=nome)) +
  geom_bar(fill=single) +
  labs(title="Atores cujas indicações foram impugnadas",
       subtitle=subtitle,
       y="", x="Número de impugnações") +
  scale_x_continuous(breaks=0:2) +
  facet_grid(~tipo_relacao)

save_plot("25_impugnacoes_por_ator.png")

### 26 - Autores mais citados

df_citacoes <- read_xlsx("input/2024_12_16_arbitragem.xlsx", sheet="citacoes_em_documentos")

df_citacoes <- df_citacoes %>%
  mutate(id_documento = case_when(
    id_documento == "35.4" ~ "35.04",
    id_documento == "1.10" ~ "1.1",
    TRUE ~ id_documento
  ))

df_documentos_sentencas <- df_documentos_expandida %>%
  filter(macro_tipo == "Sentença") %>%
  select(id_documento, macro_tipo, id_arbitragem) %>%
  mutate(id_arbitragem = ifelse(id_arbitragem == "56", "55", id_arbitragem)) # fixing missing IDs

df_citacoes_sentenças <- df_citacoes %>%
  inner_join(df_documentos_sentencas, by="id_documento")

df_citacoes_sentenças <- df_citacoes_sentenças %>%
  mutate(autor_citado = str_split(autor_citado, ";")) %>%
  unnest(autor_citado) %>%
  mutate(autor_citado = autor_citado %>%
           str_to_lower() %>%
           str_squish(),
         autor_citado = case_when(
           autor_citado == "pontes de miranda" ~ "francisco cavalcanti pontes de miranda",
           TRUE ~ autor_citado
         ) %>% 
           str_to_title() %>%
           str_replace(" De ", " de ") %>%
           str_replace(" Da ", " da ") %>%
           str_replace(" Das ", " das ") %>%
           str_replace(" Do ", " do ") %>%
           str_replace(" Dos ", " dos ")
         )

autores_mais_citados_sentencas <- df_citacoes_sentenças %>%
  count(autor_citado) %>% 
  filter(n>4) %>% 
  .$autor_citado
  
df_citacoes_sentenças %>%
  filter(autor_citado %in% autores_mais_citados_sentencas) %>%
  left_join(df_arbitragens, by="id_arbitragem") %>%
  mutate(autor_citado = fct_infreq(autor_citado) %>% fct_rev()) %>% 
  group_by(autor_citado, uf_arbitragem) %>% 
  count_and_perc() %>%
  mutate(lbl_n = as.character(n)) %>% 
  plot_fillcol(
    "autor_citado",
    "uf_arbitragem",
    title_label="Autores mais citados nas sentenças, por UF da arbitragem",
    x_label="Autor",
    y_label="Número de citações em sentenças arbitrais",
    fill_label="Unidade Federativa: ",
    fill_position="stack",
    flip=TRUE,
    order_bars=FALSE,
    fill_colors=pal_uf,
    var_label="lbl_n",
    lbl_colors="white",
    theme_func=theme_bw
  ) +
  theme(legend.position="bottom")

save_plot("26_autores_arbitragens.png")

## Valor da causa ----

### 27 - Valor da causa por setor

df_valor_da_causa <- df_arbitragens %>% 
  filter(valor_da_causa != "N/D") %>% 
  mutate(valor_causa_numerico = as.numeric(valor_da_causa))

df_valor_da_causa %>% 
  filter(setor != "N/A") %>%   
  mutate(setor = fct_lump_min(setor, 2, other_level = "Outros Setores") %>% 
           fct_reorder(valor_causa_numerico) %>% 
           fct_relevel("Outros Setores", after = 0)) %>% 
  ggplot(aes(y = setor, x = valor_causa_numerico / 1000000)) +
  geom_boxplot(color=single, fill = pal_uf[3]) +
  labs(title = "Valores das causas, por setor",
       subtitle = subtitle,
       y = "Setor", x = "Valor da causa (Milhões de R$)")

save_plot("27_valor_causa_por_setor_todos.png")

### 28 - Valor da causa por setor sem telecom

df_valor_da_causa %>% 
  filter(setor != "N/A" & setor != "Telecomunicações") %>%   
  mutate(setor = fct_lump_min(setor, 2, other_level = "Outros Setores") %>% 
           fct_reorder(valor_causa_numerico) %>% 
           fct_relevel("Outros Setores", after = 0)) %>% 
  ggplot(aes(y = setor, x = valor_causa_numerico / 1000000)) +
  geom_boxplot(fill = pal_uf[3], color=single) +
  labs(title = "Valores das causas, por setor, excluindo telecomunicações",
       subtitle = subtitle,
       y = "Setor (exceto Telecomunicações)", x = "Valor da causa (Milhões de R$)")

save_plot("28_valor_causa_por_setor_exceto_telecom.png")

### 29 - Valor da causa: telecom vs. demais

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
  geom_boxplot(color = single, fill = pal_uf[3]) +
  geom_jitter(color = pal_uf[1], alpha = 0.5, shape=8) +
  labs(title = "Valor da causa: Telecomunicações VS. Demais Setores",
       subtitle = subtitle,
       y = "Setor", x = "Valor da causa (Milhões de R$)")

save_plot("29_valor_da_causa_telecom_x_demais_setores.png")
 
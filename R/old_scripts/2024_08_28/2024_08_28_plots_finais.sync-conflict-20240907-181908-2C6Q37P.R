library(tidyverse)
library(lubridate)
library(readxl)

# Functions ----

save_plot <- function(filename, path = "./2024_08_28/output/plots", height = 6, width = 10, dpi = 300, ...) {
  ggsave(filename=filename, height=height, width=width, path=path, dpi=dpi, ...)
}

export_table <- function(df, filename, folder = "./2024_08_28/output/tabelas/") {
  df %>%
    write_excel_csv2(paste0(folder, filename))
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

# Data ----

## Arbitragens ----

df_arbitragens <- read_xlsx("2024_08_28/input/arbitragrem.xlsx", sheet = "arbitragens", col_types = rep("text", 31)) %>% 
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
  
## Atores ----

df_atores <- read_xlsx("2024_08_28/input/atores.xlsx", col_types=rep("text", 8))

relacao_ator_arbitragem <- read_xlsx(
  "2024_08_28/input/arbitragrem.xlsx",
  sheet = "relacao_ator_arbitragem",
  col_types=rep("text", 9)
) %>% 
  mutate(tipo_relacao_consolidada = case_when(
    str_detect(tipo_relacao, "Árbitro") ~ "Árbitro",
    str_detect(tipo_relacao, "Escritório") ~ "Escritório de Advocacia",
    TRUE ~ tipo_relacao
  )) %>%
  left_join(df_atores, by="id_ator") %>%
  left_join(df_arbitragens, by="id_arbitragem")

relacao_ator_arbitragem_clean <- relacao_ator_arbitragem %>% 
  filter(!is.na(nome)) %>% 
  filter(!is.na(uf_arbitragem)) %>% 
  mutate(sexo=ifelse(is.na(sexo), "masculino", sexo), # Todos os NAs tinham nomes de homens
         sexo=ifelse(str_detect(str_to_lower(sexo), "fem"),
                     "Feminino", "Masculino"))

relacao_ator_arbitragem_clean_arbitros <- relacao_ator_arbitragem_clean %>% 
  filter(tipo_relacao_consolidada == "Árbitro")

## Plots ----

theme_set({{theme_bw() + theme(legend.position = "bottom")}})

pal <- colorspace::choose_palette()
single <- pal(5)[2]

subtitle <- "Pesquisa de arbitragens (2002-2024)"

## 2. Arbitragem no tempo ----

count_resultados <- df_arbitragens %>%
  filter(!is.na(dias_ate_fim)) %>%
  count(resultado_final) %>% 
  mutate(resultado_final_n = paste0(resultado_final, " (N = ", n, ")"))

df_arbitragens %>%
  select(resultado_final, dias_ate_fim) %>% 
  filter(!is.na(dias_ate_fim)) %>% 
  left_join(count_resultados, by="resultado_final") %>% 
  ggplot(aes(y = fct_reorder(resultado_final_n, dias_ate_fim) %>% fct_rev(),
             x = dias_ate_fim)) +
  geom_boxplot(fill = single) +
  labs(title = "Tempo até o fim da arbitragem, por Resultado Final",
       subtitle = subtitle,
       y = "Resultado Final",
       x = "Tempo, em dias, até o fim da arbitragem")

save_plot("2.1. Tempo decorrido x resultado final (todas).png")

df_arbitragens %>%
  select(resultado_final, dias_ate_fim) %>% 
  filter(!is.na(dias_ate_fim)) %>% 
  left_join(count_resultados, by="resultado_final") %>% 
  ggplot(aes(y = fct_reorder(resultado_final_n, dias_ate_fim) %>% fct_rev(),
             x = dias_ate_fim)) +
  geom_boxplot(fill = single) +
  scale_x_continuous(limits = c(0, 2100)) +
  labs(title = "Tempo até o fim da arbitragem, por Resultado Final",
       subtitle = subtitle,
       y = "Resultado Final",
       x = "Tempo, em dias, até o fim da arbitragem")

save_plot("2.1. Tempo decorrido x resultado final (sem mostrar outliers).png")

count_resultados_acordo <- df_arbitragens %>%
  mutate(resultado_acordo = ifelse(resultado_final=="Acordo",
                                   "Acordo", "Outros Resultados")) %>%
  filter(!is.na(dias_ate_fim)) %>%
  count(resultado_acordo) %>% 
  mutate(resultado_acordo_n = paste0(resultado_acordo, " (N = ", n, ")"))
  
  
df_arbitragens %>% 
  select(resultado_final, dias_ate_fim) %>% 
  filter(!is.na(dias_ate_fim)) %>% 
  mutate(resultado_acordo = ifelse(resultado_final=="Acordo",
                                   "Acordo", "Outros Resultados")) %>%
  left_join(count_resultados_acordo, by="resultado_acordo") %>% 
  ggplot(aes(y = resultado_acordo_n, x = dias_ate_fim)) +
  geom_boxplot(fill = single) +
  scale_x_continuous(limits = c(0, 2100)) +
  labs(title = "Tempo até o fim da arbitragem: Acordo vs. Outros Resultados",
       subtitle = subtitle,
       y = "Resultado Final (Acordo x Outros)",
       x = "Tempo, em dias, até o fim da arbitragem")

save_plot("2.1. Tempo decorrido x houve acordo (sem mostrar outliers).png")

df_arbitragens_objetos %>%
  filter(!is.na(dias_ate_fim)) %>% 
  mutate(objeto=paste("Se há", str_to_lower(objeto)) %>% 
           str_replace("nulidade", "pedido de nulidade") %>% 
           str_replace("prorrogação", "pedido de prorrogação")) %>%
  ggplot(aes(y=fct_reorder(objeto, dias_ate_fim), x=dias_ate_fim)) +
  geom_boxplot(fill=single) +
  labs(title="Tempo das arbitragens por objeto",
       subtitle=subtitle, y="Caso",
       x="Tempo, em dias, até o fim da arbitragem")

save_plot("2.2. Tempo das arbitragens por objeto (todas).png")

df_arbitragens_objetos %>%
  filter(!is.na(dias_ate_fim)) %>% 
  mutate(objeto=paste("Se há", str_to_lower(objeto)) %>% 
           str_replace("nulidade", "pedido de nulidade") %>% 
           str_replace("prorrogação", "pedido de prorrogação")) %>%
  ggplot(aes(y=fct_reorder(objeto, dias_ate_fim), x=dias_ate_fim)) +
  geom_boxplot(fill=single) +
  scale_x_continuous(limits = c(0, 2100)) +
  labs(title="Tempo das arbitragens por objeto",
       subtitle=subtitle, y="Caso",
       x="Tempo, em dias, até o fim da arbitragem")

save_plot("2.2. Tempo das arbitragens por objeto (sem mostrar outliers).png")

## 3. Arbitragem e seus Atores ----

relacao_ator_arbitragem_clean_arbitros %>% 
  mutate(nome=fct_lump_min(nome, 3) %>% fct_infreq()) %>%
  filter(nome!="Other") %>% 
  ggplot(aes(y=fct_rev(nome), fill=uf_arbitragem)) +
  geom_bar() +
  labs(title="Atores que mais participaram como árbitros das arbitragens",
       subtitle=subtitle, x="Número de Arbitragens",
       y="Árbitro", fill="Unidade Federativa: ") +
  scale_fill_manual(values=pal(4)) +
  scale_x_continuous(breaks=seq(0, 10, 2))

save_plot("3.1. Principais árbitros.png")

relacao_ator_arbitragem_clean %>%
  filter(tipo_relacao_consolidada == "Escritório de Advocacia") %>% 
  mutate(nome=str_remove(nome, "(Sociedade de)? A[dD]vogados|Advocacia") %>% str_squish()) %>% 
  count(nome, tipo_relacao, sort=T) %>% 
  mutate(tipo_relacao=tipo_relacao %>% str_extract("representando.+") %>% str_remove("representando\\s+\\S+\\s+")) %>% 
  group_by(nome) %>% 
  summarise(`Representação`=paste(tipo_relacao, collapse="; "),
            `Número de Arbitragens`=sum(n)) %>% 
  rename(`Escritório de Advocacia`="nome") %>% 
  arrange(desc(`Número de Arbitragens`)) %>% 
  export_table("escritorios.csv")

relacao_ator_arbitragem_clean_arbitros %>% 
  mutate(idade=as.numeric(idade)) %>% 
  filter(!is.na(idade)) %>%
  ggplot(aes(x=idade, fill=sexo)) +
  geom_histogram(color="black", bins=10) +
  scale_fill_manual(values=pal(5)[c(4, 2)]) +
  labs(title="Idade e sexo dos árbitros",
       subtitle=subtitle,
       x="Idade", y="Número de Atuações de Árbitros",
       fill="Sexo: ")

save_plot("3.3. Idades e Sexos dos Árbitros.png")

relacao_ator_arbitragem_clean %>%
  filter(!is.na(sexo) & tipo_relacao_consolidada!="Escritório de Advocacia") %>%
  mutate(sexo=factor(sexo, ordered=T, levels=c("Masculino", "Feminino"))) %>% 
  group_by(tipo_relacao_consolidada, sexo) %>% 
  summarise(n=n()) %>% 
  mutate(perc=n/sum(n),
         lbl_perc=scales::label_percent(accuracy=0.1, decimal.mark=",")(perc)) %>%
  group_by(tipo_relacao_consolidada) %>% 
  complete(sexo, fill=list(n=0, perc=0, lbl_perc="0,0%")) %>%
  mutate(n_total=sum(n),
         tipo_relacao_consolidada_n = paste0(
           tipo_relacao_consolidada, " (N = ", n_total, ")"
         )) %>% 
  filter(sexo=="Feminino") %>% 
  ggplot(aes(y=fct_reorder(tipo_relacao_consolidada_n, perc),
             x=perc, label=lbl_perc)) +
  geom_col(fill=single) +
  geom_text(size=3, fontface="bold", color=single, hjust=-0.08) +
  labs(title="Participação de mulheres nas arbitragens",
       subtitle=subtitle, x="Percentual de mulheres", y="Categoria de participante") +
  scale_x_continuous(labels=scales::label_percent(), limits=c(0, 0.2), breaks=seq(0, 0.2, 0.05))

save_plot("3.3. Participação das mulheres.png")

## 4. Arbitragem e seus Valores, Objeto e Mérito ----

df_arbitragens_objetos %>%
  count(objeto) %>% 
  mutate(perc=n/n_arbitragens,
         lbl_perc=scales::label_percent(accuracy=0.1, decimal.mark=",")(perc),
         objeto=fct_reorder(objeto, n)) %>% 
  ggplot(aes(y=objeto, x=perc, label=lbl_perc)) +
  geom_col(fill=single) +
  geom_text(size=3, fontface="bold", color=single, hjust=-0.08) +
  labs(title="Objeto das arbitragens",
       subtitle=subtitle, x="Percentual de das arbitragens\nque está relacionado ao objeto", y="Objeto da arbitragem") +
  scale_x_continuous(labels=scales::label_percent())

save_plot("4.4. Objetos das arbitragens.png")

honorarios_sucumbencia %>% 
  ggplot(aes(x=total_honorarios_sucumbencia/1000000)) +
  geom_histogram(fill=single, color="black", bins=4,
                 boundary = 0, closed = "left") +
  labs(title="Valores dos honorários de sucumbência",
       subtitle=subtitle,
       x="Valor total dos honorários de sucumbência (Milhões de R$)", y="Número arbitragens")

save_plot("4.2. Honorários de sucumbência.png")

honorarios_arbitros %>% 
  ggplot(aes(x=total_honorarios_arbitros/1000000)) +
  geom_histogram(fill=single, color="black", binwidth=0.5,
                 boundary = 0, closed = "left") +
  labs(title="Valores dos honorários dos árbitros",
       subtitle=subtitle,
       x="Valor total dos honorários dos árbitros (Milhões de R$)", y="Número arbitragens")

save_plot("4.1. Honorários dos árbitros.png")

honorarios_arbitros_discriminados %>%
  ggplot(aes(x=atribuicao_honorarios_discriminados, y=valor_honorarios_discriminados/1000)) +
  geom_boxplot(fill=single) +
  labs(title="Honorários dos árbitros, por posição do árbitro",
       subtitle=subtitle,
       x="Posição do Árbitro", y="Valor recebido de honorários (Milhares de R$)")

save_plot("4.1. Honorários dos árbitros por posição.png")

df_arbitragens_objetos %>% 
  left_join(honorarios_arbitros, by="id_arbitragem") %>%
  filter(!is.na(total_honorarios_arbitros)) %>% 
  ggplot(aes(x=fct_reorder(objeto, total_honorarios_arbitros),
             y=total_honorarios_arbitros/1000000)) +
  geom_boxplot(fill=single) +
  labs(title="Valores dos honorários dos árbitros, por grupo de objeto da arbitragem",
       subtitle=subtitle,
       y="Valor total dos honorários dos árbitros (Milhões de R$)", 
       x="Presença do objeto na arbitragem")

save_plot("4.3. Honorários dos árbitros x objeto da arbitragem.png")

counts_resultado_final <- df_arbitragens %>%
  filter(!(resultado_final %in% c("N/A", "N/D"))) %>% 
  count(resultado_final) %>% 
  rename(n_rf="n")

cat_objetos <- count(df_arbitragens_objetos, objeto)$objeto

df_arbitragens_objetos %>% 
  filter(!(resultado_final %in% c("N/A", "N/D"))) %>% 
  count(resultado_final, objeto) %>%
  mutate(objeto=factor(objeto, ordered=TRUE)) %>%
  group_by(resultado_final) %>% 
  complete(objeto,
           fill=list(n=0)) %>% 
  left_join(counts_resultado_final, by="resultado_final") %>% 
  mutate(perc=n/n_rf,
         lbl=scales::label_percent(accuracy=0.1, decimal.mark=",")(perc)) %>% 
  ggplot(aes(y=objeto, x=perc, label=lbl)) +
  geom_col(fill=single) +
  geom_text(size=3, fontface="bold", color=single, hjust=-0.08) +
  labs(title="Percentuais dos objetos, por resultado final",
       subtitle=subtitle, x="Percentual das arbitragens\nque está relacionado ao objeto", y="Objeto da arbitragem") +
  scale_x_continuous(labels=scales::label_percent()) +
  facet_wrap(~resultado_final)
  
save_plot("4.4. Objeto das arbitragens por resultado final.png")

## 5. A Arbitragem processada ----

impugna <- relacao_ator_arbitragem_clean_arbitros %>% 
  select(
    id_arbitragem,
    houve_impugnacao,
    impugnacao_bem_sucedida,
    houve_renuncia,
    houve_recusa,
    momento_impugnacao
  ) %>% 
  mutate(
    houve_impugnacao = factor(
      houve_impugnacao,
      ordered = TRUE,
      levels = categorias_sim_nao
    ),
    impugnacao_bem_sucedida = factor(
      impugnacao_bem_sucedida,
      ordered = TRUE,
      levels = categorias_sim_nao
    ),
    houve_renuncia = factor(
      houve_renuncia,
      ordered = TRUE,
      levels = categorias_sim_nao
    ),
    houve_recusa = factor(
      houve_recusa,
      ordered = TRUE,
      levels = categorias_sim_nao
    )
  )

process_cats <- c(
  "Houve impugnação?",
  "A impugnação foi bem-sucedida?",
  "Houve recusa?",
  "Houve renúncia?"
)

impugna %>% 
  pivot_longer(cols=c(houve_impugnacao, impugnacao_bem_sucedida, houve_recusa, houve_renuncia)) %>% 
  mutate(pergunta=case_when(
    str_detect(name, "sucedida") ~ process_cats[2],
    str_detect(name, "impugnacao") ~ process_cats[1],
    str_detect(name, "renuncia") ~ process_cats[4],
    str_detect(name, "recusa") ~ process_cats[3]
  ) %>% factor(ordered=T, levels=process_cats)) %>% 
  group_by(pergunta, value) %>% 
  summarise(n=n()) %>% 
  mutate(perc=n/sum(n),
         perc_text=scales::label_percent(accuracy=0.1, decimal.mark=",")(perc),
         lbl=paste0(n, " (", perc_text, ")"),
         fill_case=(value %in% categorias_sim_nao[3:4])) %>% 
  ggplot(aes(x=value, n, label=lbl, fill=fill_case, color=fill_case)) +
  geom_col() +
  geom_text(size=3, fontface="bold", vjust=-0.15) +
  facet_wrap(~pergunta) +
  labs(title="Impugnações, recusas e renúncias no processo de seleção dos árbitros",
       subtitle=subtitle,
       x="", y="Número de Árbitros") +
  scale_fill_manual(values=c(single, "darkgray")) +
  scale_color_manual(values=c(single, "darkgray")) +
  theme(legend.position="none") +
  scale_y_continuous(limits=c(0,155))

save_plot("5.2-3. Impugnações, renúncias e recusas dos árbitros.png")



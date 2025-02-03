library(tidyverse)
library(readxl)

save_plot <- function(filename, path = "./2024_05_23/plots", height = 6, width = 10, dpi = 300, ...) {
  ggsave(filename=filename, height=height, width=width, path=path, dpi=dpi, ...)
}

## Sheet de arbitragens ----

frederico <- read_xlsx("2024_05_23/frederico.xlsx", sheet = "arbitragens", col_types = rep("text", 27)) %>%
  mutate(pesquisador = "frederico",
         data_inicio = ifelse(str_detect(data_inicio, "^\\d+$"), 
                              as_date(as.numeric(data_inicio), origin = "1899-12-30") %>% as.character(),
                              data_inicio),
         data_termo = ifelse(str_detect(data_termo, "^\\d+$"), 
                             as_date(as.numeric(data_termo), origin = "1899-12-30") %>% as.character(),
                             data_termo),
         data_fim = ifelse(str_detect(data_fim, "^\\d+$"), 
                           as_date(as.numeric(data_fim), origin = "1899-12-30") %>% as.character(),
                           data_fim)) %>%
  filter(uf_arbitragem == "Federal")

julia <- read_xlsx("2024_05_23/julia.xlsx", sheet = "arbitragens", col_types = rep("text", 27)) %>%
  mutate(pesquisador = "julia",
         data_inicio = ifelse(str_detect(data_inicio, "^\\d+$"), 
                              as_date(as.numeric(data_inicio), origin = "1899-12-30") %>% as.character(),
                              data_inicio),
         data_termo = ifelse(str_detect(data_termo, "^\\d+$"), 
                             as_date(as.numeric(data_termo), origin = "1899-12-30") %>% as.character(),
                             data_termo),
         data_fim = ifelse(str_detect(data_fim, "^\\d+$"), 
                           as_date(as.numeric(data_fim), origin = "1899-12-30") %>% as.character(),
                           data_fim)) %>%
  filter(!is.na(uf_arbitragem))

soraya <- read_xlsx("2024_05_23/soraya.xlsx", sheet = "arbitragens", col_types = rep("text", 28)) %>%
  mutate(pesquisador = "soraya",
         data_inicio = ifelse(str_detect(data_inicio, "^\\d+$"), 
                              as_date(as.numeric(data_inicio), origin = "1899-12-30") %>% as.character(),
                              data_inicio),
         data_termo = ifelse(str_detect(data_termo, "^\\d+$"), 
                             as_date(as.numeric(data_termo), origin = "1899-12-30") %>% as.character(),
                             data_termo),
         data_fim = ifelse(str_detect(data_fim, "^\\d+$"), 
                           as_date(as.numeric(data_fim), origin = "1899-12-30") %>% as.character(),
                           data_fim)) %>%
  select(!...28) %>%
  filter(!is.na(uf_arbitragem))


daniel <- read_xlsx("2024_05_23/daniel.xlsx", sheet = "arbitragens", col_types = rep("text", 27)) %>%
  filter(!is.na(uf_arbitragem)) %>%
  mutate(pesquisador = "daniel",
         data_inicio = ifelse(str_detect(data_inicio, "\\d"), dmy(data_inicio) %>% as.character(), data_inicio))

gabriela <- read_xlsx("2024_05_23/gabriela.xlsx", sheet = "arbitragens", col_types = rep("text", 27)) %>%
  mutate(pesquisador = "gabriela",
         data_inicio = ifelse(str_detect(data_inicio, "^\\d+$"), 
                              as_date(as.numeric(data_inicio), origin = "1899-12-30") %>% as.character(),
                              data_inicio),
         data_termo = ifelse(str_detect(data_termo, "^\\d+$"), 
                             as_date(as.numeric(data_termo), origin = "1899-12-30") %>% as.character(),
                             data_termo),
         data_fim = ifelse(str_detect(data_fim, "^\\d+$"), 
                           as_date(as.numeric(data_fim), origin = "1899-12-30") %>% as.character(),
                           data_fim)) %>%
  filter(!is.na(uf_arbitragem))

df_arbitragens <- bind_rows(frederico, julia, soraya, daniel)

# Plots Gerais ----

arbitragens_mais_de_um_preenchimento <- df_arbitragens %>%
  count(id_arbitragem) %>%
  filter(n > 1) %>%
  .$id_arbitragem
  
arbitragens_apenas_um_preenchimento <- df_arbitragens %>%
  count(id_arbitragem) %>%
  filter(n == 1) %>%
  .$id_arbitragem

arbitragens_nao_preenchidas <- as.character(1:56)[!(as.character(1:56) %in% df_arbitragens$id_arbitragem)]

tibble(
  caso = c("Mais de um preenchimento", "Apenas um preenchimento", "Nenhum preenchimento"),
  n = c(length(arbitragens_mais_de_um_preenchimento), length(arbitragens_apenas_um_preenchimento), length(arbitragens_nao_preenchidas))
) %>%
  ggplot(aes(x = caso, y = n)) +
  geom_col(fill = "lightblue") +
  labs(title = "Nível de preenchimento da aba de arbitragens",
       x = "Caso", y = "Quantidade") +
  theme_bw()

save_plot("1_nivel_preenchimento.png")

df_arbitragens %>%
  ggplot(aes(x = fct_infreq(str_to_upper(pesquisador)))) +
  geom_bar(fill = "lightblue") +
  labs(title = "Contribuições por pesquisador",
       x = "Pesquisador", y = "Número de preenchimentos na aba de arbitragens") +
  theme_bw()

save_plot("2_contribuicoes_pesquisador.png")

df_arbitragens %>%
  count(id_arbitragem, uf_arbitragem) %>%
  mutate(caso = ifelse(n > 1, "Mais de um preenchimento", "Apenas um preenchimento")) %>%
  ggplot(aes(x = uf_arbitragem, fill = caso)) +
  geom_bar() +
  scale_fill_manual(values = c("lightblue", "darkblue")) +
  labs(title = "Preenchimentos da aba arbitragem, por UF e grau",
       x = "Unidade Federativa (UF)", y = "Número de arbitragens preenchidas", fill = "Grau de preenchimento") +
  theme_bw()

save_plot("3_preenchimento_por_uf.png")

# Data de início ----

ids_divergencias_data_inicio <- df_arbitragens %>%
  count(id_arbitragem, data_inicio) %>%
  count(id_arbitragem) %>%
  filter(n > 1) %>%
  mutate(coluna = "data_inicio")

divergencias_data_inicio <- df_arbitragens %>%
  select(pesquisador, id_arbitragem, data_inicio) %>%
  filter(id_arbitragem %in% ids_divergencias_data_inicio$id_arbitragem) %>%
  arrange(id_arbitragem) %>%
  rename(preenchimento = "data_inicio") %>%
  mutate(coluna = "data_inicio")

casos_divergencias_data_inicio <- divergencias_data_inicio %>%
  group_by(id_arbitragem) %>%
  summarise(pesquisadores = paste(pesquisador, collapse = " x ")) %>%
  mutate(coluna = "data_inicio")

# Data de termo ----

ids_divergencias_data_termo <- df_arbitragens %>%
  count(id_arbitragem, data_termo) %>%
  count(id_arbitragem) %>%
  filter(n > 1) %>%
  mutate(coluna = "data_termo")

divergencias_data_termo <- df_arbitragens %>%
  select(pesquisador, id_arbitragem, data_termo) %>%
  filter(id_arbitragem %in% ids_divergencias_data_termo$id_arbitragem) %>%
  arrange(id_arbitragem) %>%
  rename(preenchimento = "data_termo") %>%
  mutate(coluna = "data_termo")

casos_divergencias_data_termo <- divergencias_data_termo %>%
  group_by(id_arbitragem) %>%
  summarise(pesquisadores = paste(pesquisador, collapse = " x ")) %>%
  mutate(coluna = "data_termo")

# Data de fim ----

ids_divergencias_data_fim <- df_arbitragens %>%
  count(id_arbitragem, data_fim) %>%
  count(id_arbitragem) %>%
  filter(n > 1) %>%
  mutate(coluna = "data_fim")

divergencias_data_fim <- df_arbitragens %>%
  select(pesquisador, id_arbitragem, data_fim) %>%
  filter(id_arbitragem %in% ids_divergencias_data_fim$id_arbitragem) %>%
  arrange(id_arbitragem) %>%
  rename(preenchimento = "data_fim") %>%
  mutate(coluna = "data_fim")

casos_divergencias_data_fim <- divergencias_data_fim %>%
  group_by(id_arbitragem) %>%
  summarise(pesquisadores = paste(pesquisador, collapse = " x ")) %>%
  mutate(coluna = "data_fim")

# Pólo Ativo ----

ids_divergencias_polo_ativo <- df_arbitragens %>%
  count(id_arbitragem, polo_ativo) %>%
  count(id_arbitragem) %>%
  filter(n > 1) %>%
  mutate(coluna = "polo_ativo")

divergencias_polo_ativo <- df_arbitragens %>%
  select(pesquisador, id_arbitragem, polo_ativo) %>%
  filter(id_arbitragem %in% ids_divergencias_polo_ativo$id_arbitragem) %>%
  arrange(id_arbitragem) %>%
  rename(preenchimento = "polo_ativo") %>%
  mutate(coluna = "polo_ativo")

casos_divergencias_polo_ativo <- divergencias_polo_ativo %>%
  group_by(id_arbitragem) %>%
  summarise(pesquisadores = paste(pesquisador, collapse = " x ")) %>%
  mutate(coluna = "polo_ativo")


# Pólo Passivo ----

ids_divergencias_polo_passivo <- df_arbitragens %>%
  count(id_arbitragem, polo_passivo) %>%
  count(id_arbitragem) %>%
  filter(n > 1) %>%
  mutate(coluna = "polo_passivo")

divergencias_polo_passivo <- df_arbitragens %>%
  select(pesquisador, id_arbitragem, polo_passivo) %>%
  filter(id_arbitragem %in% ids_divergencias_polo_passivo$id_arbitragem) %>%
  arrange(id_arbitragem) %>%
  rename(preenchimento = "polo_passivo") %>%
  mutate(coluna = "polo_passivo")

casos_divergencias_polo_passivo <- divergencias_polo_passivo %>%
  group_by(id_arbitragem) %>%
  summarise(pesquisadores = paste(pesquisador, collapse = " x ")) %>%
  mutate(coluna = "polo_passivo")

# Setor ----

ids_divergencias_setor <- df_arbitragens %>%
  count(id_arbitragem, setor) %>%
  count(id_arbitragem) %>%
  filter(n > 1) %>%
  mutate(coluna = "setor")

divergencias_setor <- df_arbitragens %>%
  select(pesquisador, id_arbitragem, setor) %>%
  filter(id_arbitragem %in% ids_divergencias_setor$id_arbitragem) %>%
  arrange(id_arbitragem) %>%
  rename(preenchimento = "setor") %>%
  mutate(coluna = "setor")

casos_divergencias_setor <- divergencias_setor %>%
  group_by(id_arbitragem) %>%
  summarise(pesquisadores = paste(pesquisador, collapse = " x ")) %>%
  mutate(coluna = "setor")

# Objeto ----

ids_divergencias_objeto <- df_arbitragens %>%
  count(id_arbitragem, objeto) %>%
  count(id_arbitragem) %>%
  filter(n > 1) %>%
  mutate(coluna = "objeto")

divergencias_objeto <- df_arbitragens %>%
  select(pesquisador, id_arbitragem, objeto) %>%
  filter(id_arbitragem %in% ids_divergencias_objeto$id_arbitragem) %>%
  arrange(id_arbitragem) %>%
  rename(preenchimento = "objeto") %>%
  mutate(coluna = "objeto")

casos_divergencias_objeto <- divergencias_objeto %>%
  group_by(id_arbitragem) %>%
  summarise(pesquisadores = paste(pesquisador, collapse = " x ")) %>%
  mutate(coluna = "objeto")

# Houve Reconvenção ----

ids_divergencias_houve_reconvencao <- df_arbitragens %>%
  count(id_arbitragem, houve_reconvencao) %>%
  count(id_arbitragem) %>%
  filter(n > 1) %>%
  mutate(coluna = "houve_reconvencao")

divergencias_houve_reconvencao <- df_arbitragens %>%
  select(pesquisador, id_arbitragem, houve_reconvencao) %>%
  filter(id_arbitragem %in% ids_divergencias_houve_reconvencao$id_arbitragem) %>%
  arrange(id_arbitragem) %>%
  rename(preenchimento = "houve_reconvencao") %>%
  mutate(coluna = "houve_reconvencao")

casos_divergencias_houve_reconvencao <- divergencias_houve_reconvencao %>%
  group_by(id_arbitragem) %>%
  summarise(pesquisadores = paste(pesquisador, collapse = " x ")) %>%
  mutate(coluna = "houve_reconvencao")

# Valor da causa ----

ids_divergencias_valor_da_causa <- df_arbitragens %>%
  count(id_arbitragem, valor_da_causa) %>%
  count(id_arbitragem) %>%
  filter(n > 1) %>%
  mutate(coluna = "valor_da_causa")

divergencias_valor_da_causa <- df_arbitragens %>%
  select(pesquisador, id_arbitragem, valor_da_causa) %>%
  filter(id_arbitragem %in% ids_divergencias_valor_da_causa$id_arbitragem) %>%
  arrange(id_arbitragem) %>%
  rename(preenchimento = "valor_da_causa") %>%
  mutate(coluna = "valor_da_causa")

casos_divergencias_valor_da_causa <- divergencias_valor_da_causa %>%
  group_by(id_arbitragem) %>%
  summarise(pesquisadores = paste(pesquisador, collapse = " x ")) %>%
  mutate(coluna = "valor_da_causa")

# Câmara ----

ids_divergencias_camara <- df_arbitragens %>%
  count(id_arbitragem, camara) %>%
  count(id_arbitragem) %>%
  filter(n > 1) %>%
  mutate(coluna = "camara")

divergencias_camara <- df_arbitragens %>%
  select(pesquisador, id_arbitragem, camara) %>%
  filter(id_arbitragem %in% ids_divergencias_camara$id_arbitragem) %>%
  arrange(id_arbitragem) %>%
  rename(preenchimento = "camara") %>%
  mutate(coluna = "camara")

casos_divergencias_camara <- divergencias_camara %>%
  group_by(id_arbitragem) %>%
  summarise(pesquisadores = paste(pesquisador, collapse = " x ")) %>%
  mutate(coluna = "camara")

# Transparência ----

ids_divergencias_transparencia <- df_arbitragens %>%
  count(id_arbitragem, transparencia) %>%
  count(id_arbitragem) %>%
  filter(n > 1) %>%
  mutate(coluna = "transparencia")

divergencias_transparencia <- df_arbitragens %>%
  select(pesquisador, id_arbitragem, transparencia) %>%
  filter(id_arbitragem %in% ids_divergencias_transparencia$id_arbitragem) %>%
  arrange(id_arbitragem) %>%
  rename(preenchimento = "transparencia") %>%
  mutate(coluna = "transparencia")

casos_divergencias_transparencia <- divergencias_transparencia %>%
  group_by(id_arbitragem) %>%
  summarise(pesquisadores = paste(pesquisador, collapse = " x ")) %>%
  mutate(coluna = "transparencia")

# Terminada ----

ids_divergencias_terminada <- df_arbitragens %>%
  count(id_arbitragem, terminada) %>%
  count(id_arbitragem) %>%
  filter(n > 1) %>%
  mutate(coluna = "terminada")

divergencias_terminada <- df_arbitragens %>%
  select(pesquisador, id_arbitragem, terminada) %>%
  filter(id_arbitragem %in% ids_divergencias_terminada$id_arbitragem) %>%
  arrange(id_arbitragem) %>%
  rename(preenchimento = "terminada") %>%
  mutate(coluna = "terminada")

casos_divergencias_terminada <- divergencias_terminada %>%
  group_by(id_arbitragem) %>%
  summarise(pesquisadores = paste(pesquisador, collapse = " x ")) %>%
  mutate(coluna = "terminada")

# Teve Sentença Parcial ----

ids_divergencias_teve_sentenca_parcial <- df_arbitragens %>%
  count(id_arbitragem, teve_sentenca_parcial) %>%
  count(id_arbitragem) %>%
  filter(n > 1) %>%
  mutate(coluna = "teve_sentenca_parcial")

divergencias_teve_sentenca_parcial <- df_arbitragens %>%
  select(pesquisador, id_arbitragem, teve_sentenca_parcial) %>%
  filter(id_arbitragem %in% ids_divergencias_teve_sentenca_parcial$id_arbitragem) %>%
  arrange(id_arbitragem) %>%
  rename(preenchimento = "teve_sentenca_parcial") %>%
  mutate(coluna = "teve_sentenca_parcial")

casos_divergencias_teve_sentenca_parcial <- divergencias_teve_sentenca_parcial %>%
  group_by(id_arbitragem) %>%
  summarise(pesquisadores = paste(pesquisador, collapse = " x ")) %>%
  mutate(coluna = "teve_sentenca_parcial")

# Resultado da Sentença Parcial ----

ids_divergencias_resultado_sentenca_parcial <- df_arbitragens %>%
  count(id_arbitragem, resultado_sentenca_parcial) %>%
  count(id_arbitragem) %>%
  filter(n > 1) %>%
  mutate(coluna = "resultado_sentenca_parcial")

divergencias_resultado_sentenca_parcial <- df_arbitragens %>%
  select(pesquisador, id_arbitragem, resultado_sentenca_parcial) %>%
  filter(id_arbitragem %in% ids_divergencias_resultado_sentenca_parcial$id_arbitragem) %>%
  arrange(id_arbitragem) %>%
  rename(preenchimento = "resultado_sentenca_parcial") %>%
  mutate(coluna = "resultado_sentenca_parcial")

casos_divergencias_resultado_sentenca_parcial <- divergencias_resultado_sentenca_parcial %>%
  group_by(id_arbitragem) %>%
  summarise(pesquisadores = paste(pesquisador, collapse = " x ")) %>%
  mutate(coluna = "resultado_sentenca_parcial")

# Resultado Final ----

ids_divergencias_resultado_final <- df_arbitragens %>%
  count(id_arbitragem, resultado_final) %>%
  count(id_arbitragem) %>%
  filter(n > 1) %>%
  mutate(coluna = "resultado_final")

divergencias_resultado_final <- df_arbitragens %>%
  select(pesquisador, id_arbitragem, resultado_final) %>%
  filter(id_arbitragem %in% ids_divergencias_resultado_final$id_arbitragem) %>%
  arrange(id_arbitragem) %>%
  rename(preenchimento = "resultado_final") %>%
  mutate(coluna = "resultado_final")

casos_divergencias_resultado_final <- divergencias_resultado_final %>%
  group_by(id_arbitragem) %>%
  summarise(pesquisadores = paste(pesquisador, collapse = " x ")) %>%
  mutate(coluna = "resultado_final")

# Teve Liminar Pré-Arbitral ----

ids_divergencias_teve_liminar_pre_arbitral <- df_arbitragens %>%
  count(id_arbitragem, teve_liminar_pre_arbitral) %>%
  count(id_arbitragem) %>%
  filter(n > 1) %>%
  mutate(coluna = "teve_liminar_pre_arbitral")

divergencias_teve_liminar_pre_arbitral <- df_arbitragens %>%
  select(pesquisador, id_arbitragem, teve_liminar_pre_arbitral) %>%
  filter(id_arbitragem %in% ids_divergencias_teve_liminar_pre_arbitral$id_arbitragem) %>%
  arrange(id_arbitragem) %>%
  rename(preenchimento = "teve_liminar_pre_arbitral") %>%
  mutate(coluna = "teve_liminar_pre_arbitral")

casos_divergencias_teve_liminar_pre_arbitral <- divergencias_teve_liminar_pre_arbitral %>%
  group_by(id_arbitragem) %>%
  summarise(pesquisadores = paste(pesquisador, collapse = " x ")) %>%
  mutate(coluna = "teve_liminar_pre_arbitral")

# Houve Intervenção do TCU ----

ids_divergencias_houve_intervencao_tcu <- df_arbitragens %>%
  count(id_arbitragem, houve_intervencao_tcu) %>%
  count(id_arbitragem) %>%
  filter(n > 1) %>%
  mutate(coluna = "houve_intervencao_tcu")

divergencias_houve_intervencao_tcu <- df_arbitragens %>%
  select(pesquisador, id_arbitragem, houve_intervencao_tcu) %>%
  filter(id_arbitragem %in% ids_divergencias_houve_intervencao_tcu$id_arbitragem) %>%
  arrange(id_arbitragem) %>%
  rename(preenchimento = "houve_intervencao_tcu") %>%
  mutate(coluna = "houve_intervencao_tcu")

casos_divergencias_houve_intervencao_tcu <- divergencias_houve_intervencao_tcu %>%
  group_by(id_arbitragem) %>%
  summarise(pesquisadores = paste(pesquisador, collapse = " x ")) %>%
  mutate(coluna = "houve_intervencao_tcu")

# Houve Intervenção de Terceiros ----

ids_divergencias_houve_intervencao_terceiros <- df_arbitragens %>%
  count(id_arbitragem, houve_intervencao_terceiros) %>%
  count(id_arbitragem) %>%
  filter(n > 1) %>%
  mutate(coluna = "houve_intervencao_terceiros")

divergencias_houve_intervencao_terceiros <- df_arbitragens %>%
  select(pesquisador, id_arbitragem, houve_intervencao_terceiros) %>%
  filter(id_arbitragem %in% ids_divergencias_houve_intervencao_terceiros$id_arbitragem) %>%
  arrange(id_arbitragem) %>%
  rename(preenchimento = "houve_intervencao_terceiros") %>%
  mutate(coluna = "houve_intervencao_terceiros")

casos_divergencias_houve_intervencao_terceiros <- divergencias_houve_intervencao_terceiros %>%
  group_by(id_arbitragem) %>%
  summarise(pesquisadores = paste(pesquisador, collapse = " x ")) %>%
  mutate(coluna = "houve_intervencao_terceiros")

# Valor dos Honorários ----

ids_divergencias_valor_dos_honorarios <- df_arbitragens %>%
  count(id_arbitragem, valor_dos_honorarios) %>%
  count(id_arbitragem) %>%
  filter(n > 1) %>%
  mutate(coluna = "valor_dos_honorarios")

divergencias_valor_dos_honorarios <- df_arbitragens %>%
  select(pesquisador, id_arbitragem, valor_dos_honorarios) %>%
  filter(id_arbitragem %in% ids_divergencias_valor_dos_honorarios$id_arbitragem) %>%
  arrange(id_arbitragem) %>%
  rename(preenchimento = "valor_dos_honorarios") %>%
  mutate(coluna = "valor_dos_honorarios")

casos_divergencias_valor_dos_honorarios <- divergencias_valor_dos_honorarios %>%
  group_by(id_arbitragem) %>%
  summarise(pesquisadores = paste(pesquisador, collapse = " x ")) %>%
  mutate(coluna = "valor_dos_honorarios")

# Valor dos Honorários de Sucumbência ----

ids_divergencias_valor_honrarios_sucumbencia <- df_arbitragens %>%
  count(id_arbitragem, valor_honrarios_sucumbencia) %>%
  count(id_arbitragem) %>%
  filter(n > 1) %>%
  mutate(coluna = "valor_honrarios_sucumbencia")

divergencias_valor_honrarios_sucumbencia <- df_arbitragens %>%
  select(pesquisador, id_arbitragem, valor_honrarios_sucumbencia) %>%
  filter(id_arbitragem %in% ids_divergencias_valor_honrarios_sucumbencia$id_arbitragem) %>%
  arrange(id_arbitragem) %>%
  rename(preenchimento = "valor_honrarios_sucumbencia") %>%
  mutate(coluna = "valor_honrarios_sucumbencia")

casos_divergencias_valor_honrarios_sucumbencia <- divergencias_valor_honrarios_sucumbencia %>%
  group_by(id_arbitragem) %>%
  summarise(pesquisadores = paste(pesquisador, collapse = " x ")) %>%
  mutate(coluna = "valor_honrarios_sucumbencia")

# Sucumbência prevista para o Poder Público ----

ids_divergencias_sucumbencia_prevista_para_pp <- df_arbitragens %>%
  count(id_arbitragem, sucumbencia_prevista_para_pp) %>%
  count(id_arbitragem) %>%
  filter(n > 1) %>%
  mutate(coluna = "sucumbencia_prevista_para_pp")

divergencias_sucumbencia_prevista_para_pp <- df_arbitragens %>%
  select(pesquisador, id_arbitragem, sucumbencia_prevista_para_pp) %>%
  filter(id_arbitragem %in% ids_divergencias_sucumbencia_prevista_para_pp$id_arbitragem) %>%
  arrange(id_arbitragem) %>%
  rename(preenchimento = "sucumbencia_prevista_para_pp") %>%
  mutate(coluna = "sucumbencia_prevista_para_pp")

casos_divergencias_sucumbencia_prevista_para_pp <- divergencias_sucumbencia_prevista_para_pp %>%
  group_by(id_arbitragem) %>%
  summarise(pesquisadores = paste(pesquisador, collapse = " x ")) %>%
  mutate(coluna = "sucumbencia_prevista_para_pp")

# Caiu em Precatório ----

ids_divergencias_caiu_em_precatorio <- df_arbitragens %>%
  count(id_arbitragem, caiu_em_precatorio) %>%
  count(id_arbitragem) %>%
  filter(n > 1) %>%
  mutate(coluna = "caiu_em_precatorio")

divergencias_caiu_em_precatorio <- df_arbitragens %>%
  select(pesquisador, id_arbitragem, caiu_em_precatorio) %>%
  filter(id_arbitragem %in% ids_divergencias_caiu_em_precatorio$id_arbitragem) %>%
  arrange(id_arbitragem) %>%
  rename(preenchimento = "caiu_em_precatorio") %>%
  mutate(coluna = "caiu_em_precatorio")

casos_divergencias_caiu_em_precatorio <- divergencias_caiu_em_precatorio %>%
  group_by(id_arbitragem) %>%
  summarise(pesquisadores = paste(pesquisador, collapse = " x ")) %>%
  mutate(coluna = "caiu_em_precatorio")

# Junção das divergências ----

divergencias <- bind_rows(
  divergencias_data_inicio,
  divergencias_data_termo,
  divergencias_data_fim,
  divergencias_polo_ativo,
  divergencias_polo_passivo,
  divergencias_setor,
  divergencias_objeto,
  divergencias_houve_reconvencao,
  divergencias_valor_da_causa,
  divergencias_camara,
  divergencias_transparencia,
  divergencias_terminada,
  divergencias_teve_sentenca_parcial,
  divergencias_resultado_sentenca_parcial,
  divergencias_resultado_final,
  divergencias_teve_liminar_pre_arbitral,
  divergencias_houve_intervencao_tcu,
  divergencias_houve_intervencao_terceiros,
  divergencias_valor_dos_honorarios,
  divergencias_valor_honrarios_sucumbencia,
  divergencias_sucumbencia_prevista_para_pp,
  divergencias_caiu_em_precatorio
)

write_csv2(divergencias, "2024_05_23/divergencias.csv")

casos_divergencias <- bind_rows(
  casos_divergencias_data_inicio,
  casos_divergencias_data_termo,
  casos_divergencias_data_fim,
  casos_divergencias_polo_ativo,
  casos_divergencias_polo_passivo,
  casos_divergencias_setor,
  casos_divergencias_objeto,
  casos_divergencias_houve_reconvencao,
  casos_divergencias_valor_da_causa,
  casos_divergencias_camara,
  casos_divergencias_transparencia,
  casos_divergencias_terminada,
  casos_divergencias_teve_sentenca_parcial,
  casos_divergencias_resultado_sentenca_parcial,
  casos_divergencias_resultado_final,
  casos_divergencias_teve_liminar_pre_arbitral,
  casos_divergencias_houve_intervencao_tcu,
  casos_divergencias_houve_intervencao_terceiros,
  casos_divergencias_valor_dos_honorarios,
  casos_divergencias_valor_honrarios_sucumbencia,
  casos_divergencias_sucumbencia_prevista_para_pp,
  casos_divergencias_caiu_em_precatorio
)

write_csv2(casos_divergencias, "2024_05_23/casos_divergencias.csv")

# Plots divergências ----

casos_divergencias %>%
  ggplot(aes(y = fct_rev(fct_infreq(coluna)))) +
  geom_bar(fill = "lightblue") +
  labs(title = "Divergências de preenchimento por coluna",
       y = "Coluna", x = "Número de divergências") +
  theme_bw()

casos_divergencias %>%
  ggplot(aes(y = fct_rev(fct_infreq(coluna)), fill = fct_rev(pesquisadores))) +
  geom_bar() +
  labs(title = "Divergências de preenchimento por coluna",
       y = "Coluna", x = "Número de divergências",
       fill = "Pesquisadores divergentes") +
  scale_fill_manual(values = c("lightblue", "darkblue")) +
  theme_bw() +
  theme(legend.position = "bottom")

save_plot("4_divergencias_por_coluna.png")





library(tidyverse)
library(lubridate)
library(readxl)

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

df_documentos_aux <- read_xlsx("input/2024_12_16_arbitragem.xlsx", sheet = "documentos_aux", col_types = c(rep("text", 4), "numeric")) %>% 
  janitor::clean_names() %>% 
  filter(!duplicated(id_documento)) %>% 
  filter(!is.na(tipo_documento))

df_documentos <- read_xlsx("input/2024_12_16_arbitragem.xlsx", sheet = "documentos", col_types = c(rep("text", 4), "numeric")) %>% 
  janitor::clean_names()

df_documentos <- df_documentos %>%
  mutate(macro_tipo = case_when(
    str_detect(tipo_documento, "Sentença") ~ "Sentença",
    str_detect(tipo_documento, "finais") ~ "Alegações Finais",
    str_detect(tipo_documento, "Requerimento") ~ "Requerimento de Arbitragem",
    
    TRUE ~ tipo_documento
  ))

## Limpeza dos tipos de documentos ----

lista_de_tipos_normalizados <- df_documentos_aux %>% 
  mutate(tipo_documento = str_to_lower(tipo_documento) %>% 
           rm_accent() %>% 
           str_squish()) %>% 
  count(tipo_documento, sort = TRUE) %>%
  .$tipo_documento

limpeza_de_tipos <- list(
  op = list(
    cases = c("ordem procedimental", "anexo ordem processual"),
    convert = "Outros"
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
    convert = "Resposta a documento"
  ),
  rreq = list(
    cases = c("resposta ao requerimento", "resposta requerimento arbitragem"),
    convert = "Resposta a documento"
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
    convert = "Outros" 
  ),
  rarb = list(
    cases = c("requerimento arbitragem", "requerimento de arbitragem"),
    convert = "Requerimento de Arbitragem"
  ),
  ep = list(
    cases = c("especificacao provas"),
    convert = "Especificação Provas"
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
    convert = "Sentença"
  ),
  sentpar = list(
    cases = c(
      "sentenca arbitral parcial",
      "sentenca parcial",
      "sentenca parcial e decisao sobre provas"
    ),
    convert = "Sentença"
  ),
  addsent = list(
    cases = c("addendum a sentenca"),
    convert = "Sentença"
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
    convert = "Pedido"
  ),
  psus = list(
    cases = c("pedido suspensao"),
    convert = "Pedido"
  ),
  senthomolog = list(
    cases = c("sentenca homologatoria de acordo"),
    convert = "Sentença"
  ),
  addsentpar = list(
    cases = c("addendum a sentenca parcial"),
    convert = "Sentença"
  ),
  pedrec = list(
    cases = c("pedido de reconsideracao"),
    convert = "Pedido"
  ),
  comparb = list(
    cases = c("compromisso arbitral", "termo de compromisso arbitral"),
    convert = "Compromisso Arbitral"
  ),
  impugques = list(
    cases = c("impugnacao quesitos"),
    convert = "Impugnação" 
  ),
  indiqassques = list(
    cases = c("indicacao de assistentes tecnicos e quesitos"),
    convert = "Indicação de assistentes técnicos e quesitos"
  ),
  pedesc = list(
    cases = c("pedido de esclarecimento"),
    convert = "Pedido"
  ),
  resprec = list(
    cases = c("resposta reconvencao"),
    convert = "Resposta a documento"
  ),
  adita = list(
    cases = c("aditamento da ata de missao", "aditamento do requerimento"),
    convert = "Aditamento de documento"
  ),
  pedcaut = list(
    cases = c("aditamento e pedido de tutela cautelar"),
    convert = "Pedido"
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
    convert = "Alegações Finais"
  ),
  ataud = list(
    cases = c("ata de audiencia", "audiencia"),
    convert = "Ata de Audiência"
  ),
  decaut = list(
    cases = c("decisao cautelar"),
    convert = "Decisão"
  ),
  decemerg = list(
    cases = c("decisao de emergencia"),
    convert = "Decisão"
  ),
  decext = list(
    cases = c("decisao extintiva"),
    convert = "Decisão"
  ),
  decconfid = list(
    cases = c("decisao sobre confidencialidade"),
    convert = "Decisão"
  ),
  decurg = list(
    cases = c(
      "decisao sobre revogacao ou manutencao da tutela de urgencia",
      "pedido revogacao liminar"
    ),
    convert = "Decisão"
  ),
  impugpro = list(
    cases = c("impugnacao provas"),
    convert = "Impugnação"
  ),
  junt = list(
    cases = c("juntada"),
    convert = "Outros"
  ),
  notific = list(
    cases = c("notificacao sobre requerimento"),
    convert = "Notificação"
  ),
  reqaobj = list(
    cases = c("requerimento ampliacao objetiva"),
    convert = "Pedido" 
  ),
  reprevlim = list(
    cases = c("resposa revogacao liminar"),
    convert = "Resposta a documento" 
  ),
  repimpug = list(
    cases = c("resposta a impugnacao", "resposta impugnacao provas"),
    convert = "Resposta a documento" 
  ),
  respanp = list(
    cases = c("resposta da anp"),
    convert = "Resposta a documento" 
  ),
  senthomologdesist = list(
    cases = c("sentenca arbitral homologatoria de desistencia"),
    convert = "Sentença"
  ),
  sentexcuni = list(
    cases = c(
      "sentenca exclui uniao",
      "sentenca que exclui a uniao"
    ),
    convert = "Sentença"
  )
)

df_documentos_aux <- df_documentos_aux %>% 
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

df_documentos_aux_perdidos <- df_documentos_aux %>% 
  filter(!(id_documento %in% df_documentos$id_documento)) %>%
  rename(macro_tipo="categoria_de_tipo")

## Remoção de documentos duplicados ----

write_csv(df_documentos_aux_perdidos, "input/documentos_aux.csv")

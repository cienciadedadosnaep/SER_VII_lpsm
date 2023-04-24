library(readxl)


#FONTE: https://www.gov.br/inep/pt-br/acesso-a-informacao/dados-abertos/inep-data/catalogo-de-escolas
geolocalizacao_escolas <- read_excel("data/Análise - Tabela da lista das escolas - Detalhado.xlsx", 
                                     col_types = c("text", "text", "numeric", 
                                                   "text", "text", "text", "text", "text", 
                                                   "text", "text", "text", "text", "text", 
                                                   "text", "text", "text", "text", "numeric", 
                                                   "numeric"))
View(geolocalizacao_escolas)

#FONTE:  https://www.gov.br/inep/pt-br/acesso-a-informacao/dados-abertos/indicadores-educacionais/nivel-socioeconomico
INSE_2019_ESCOLAS <- read_excel("data/INSE_2019_ESCOLAS.xlsx", 
                                col_types = c("text", "text", "numeric", 
                                              "text", "numeric", "text", "numeric", 
                                              "numeric", "numeric", "numeric", 
                                              "numeric", "text", "numeric", "numeric", 
                                              "numeric", "numeric", "numeric", 
                                              "numeric", "numeric", "numeric"))
View(INSE_2019_ESCOLAS)
mapeamento_salvador <- read_excel("data/mapeamento_clube_ciencia_salvador_regiao_metropolitana.xlsx", 
                                  col_types = c("date", "text", "text", 
                                                "text", "text", "text", "text", "text", 
                                                "text", "text", "text", "text", "text", 
                                                "text", "text", "text", "text", "numeric"))

#Fonte: https://www.gov.br/inep/pt-br/areas-de-atuacao/pesquisas-estatisticas-e-indicadores/ideb/resultados
IDEB_BA_ENSINO_MEDIO <- read_excel("data/divulgacao_ensino_medio_escolas_2021_bahia.xlsx",
                                                         col_types = c("text", "text", "numeric", 
                                                                                "numeric", "text", "text", "numeric", 
                                                                                "numeric", "numeric", "numeric", 
                                                                                "numeric", "numeric", "numeric", 
                                                                                "numeric", "numeric", "numeric"))
IDEB_BA_ANOS_FINAIS <- read_excel("data/divulgacao_anos_finais_escolas_2021_bahia.xlsx", 
                                                        col_types = c("text", "numeric", "text", 
                                                                      "numeric", "text", "text", "numeric", 
                                                                      "numeric", "numeric", "numeric", 
                                                                      "numeric", "numeric", "numeric", 
                                                                      "numeric", "numeric", "numeric"))
View(divulgacao_anos_finais_escolas_2021_bahia)




View(IDEB_BA_ENSINO_MEDIO)
IDEB_BA_ENSINO_MEDIO %<>% mutate(`Código da Escola` = as.character(`Código da Escola`))
IDEB_BA_ANOS_FINAIS %<>% mutate(`Código da Escola` = as.character(`Código da Escola`))
View(mapeamento_salvador)
#install.packages("leaflet")
#install.packages("leaflet.extras")
library(dplyr)
library(leaflet)
library(leaflet.extras)
library(tidyr)



#geolocalizacao_escolas %>%
#drop_na(Latitude, Longitude)%>%
#  leaflet() %>% 
#  addTiles() %>% 
#  addProviderTiles(providers$OpenStreetMap.DE) %>% 
#  setView(-38.4368023,-12.9144042,10) %>%
#  addMarkers(lng=~Longitude,
#             lat=~Latitude, popup =~UF)

geolocalizacao_escolas %>%
  drop_na(Latitude, Longitude)%>%
  leaflet() %>%
  addTiles() %>%
  addProviderTiles(providers$OpenStreetMap.DE) %>%
  setView(-38.4368023,-12.9144042,10) %>%
  addCircles(lng=~Longitude,
             lat=~Latitude, popup = ~`Porte da Escola`)
###############################################################################
# Filtro para Salvador
INSE_2019_ESCOLAS_SSA<- INSE_2019_ESCOLAS %>% filter(CO_MUNICIPIO %in% c("2927408"))

library(magrittr)
library(htmltools)

# Conversao numeric to character
geolocalizacao_escolas %<>% mutate(`Código INEP` = as.character(`Código INEP`))


INSE_GEO_SSA<- full_join(x = INSE_2019_ESCOLAS_SSA,
                         y = geolocalizacao_escolas,
                         by = c("CO_ESCOLA"="Código INEP"))

INSE_GEO_SSA %<>% mutate(INSE_CLASSIFICACAO =as.factor(INSE_CLASSIFICACAO))
factpal <- colorFactor(topo.colors(5), as.factor(INSE_GEO_SSA$INSE_CLASSIFICACAO))

INSE_GEO_SSA %>%
  drop_na(Latitude, Longitude)%>%
  leaflet() %>%
  addTiles() %>%
  addProviderTiles(providers$OpenStreetMap.DE) %>%
  setView(-38.4368023,-12.9144042,10) %>%
  addCircles(lng=~Longitude,
             lat=~Latitude,
             radius = ~QTD_ALUNOS_INSE,
             color = ~factpal(INSE_CLASSIFICACAO),
             label = ~htmlEscape(NOME_ESCOLA))

glimpse(mapeamento_salvador)

#ESCOLAS COM CLUBE DE CIENCIAS

escolas_com_clube <- filter(mapeamento_salvador, `3-Sua Unidade Escolar atualmente tem um Clube de Ciências ativo?` == "Sim")
escolas_com_clube %<>% mutate(`Código INEP` = as.character(`Código INEP`))
INSE_GEO_CLUBE_SSA <-INSE_GEO_SSA |> 
  inner_join(escolas_com_clube, by = c("CO_ESCOLA"="Código INEP"))

INSE_GEO_CLUBE_SSA %>%
  drop_na(Latitude, Longitude)%>%
  leaflet() %>%
  addTiles() %>%
  addProviderTiles(providers$OpenStreetMap.DE) %>%
  setView(-38.4368023,-12.9144042,10) %>%
  addCircles(lng=~Longitude,
             lat=~Latitude,
             radius = ~QTD_ALUNOS_INSE,
             color = ~factpal(INSE_CLASSIFICACAO),
             label = ~htmlEscape(Escola))
#carneiro ribeiro sem inse e raul sá sem localização
#INse 4 -azul clarinho
#Inse 2 - roxo
#inse 3 - azul escuro
#INSE 7 - AMARELO
#INSE 5 - VERDE

#INSE_GEO_CLUBE_SSA
#INSE_GEO_CLUBE_SSA %<>% mutate(INSE_VALOR_ABSOLUTO = as.numeric(INSE_VALOR_ABSOLUTO))
#INSE_GEO_CLUBE_SSA %<>% drop_na(INSE_VALOR_ABSOLUTO)
#mean(INSE_GEO_CLUBE_SSA$INSE_VALOR_ABSOLUTO)

#ESCOLAS QUE RESPONDERAM A PESQUISA

mapeamento_salvador%<>% mutate(`Código INEP` = as.character(`Código INEP`))
INSE_GEO_RESPOSTA_SSA <-INSE_GEO_SSA |> 
  inner_join(mapeamento_salvador, by = c("CO_ESCOLA"="Código INEP"))

INSE_GEO_RESPOSTA_SSA %>%
  drop_na(Latitude, Longitude)%>%
  leaflet() %>%
  addTiles() %>%
  addProviderTiles(providers$OpenStreetMap.DE) %>%
  setView(-38.4368023,-12.9144042,10) %>%
  addCircles(lng=~Longitude,
             lat=~Latitude,
             radius = ~QTD_ALUNOS_INSE,
             color = ~factpal(INSE_CLASSIFICACAO),
             label = ~htmlEscape(Escola))


#------------------------ideb das escolas-------------------------------------

IDEB_Respostas_ENSINO_MEDIO <-IDEB_BA_ENSINO_MEDIO |> 
  inner_join( mapeamento_salvador, by = c("Código da Escola"="Código INEP"))
GEO_IDEB_Respostas_ENSINO_MEDIO <-IDEB_Respostas_ENSINO_MEDIO |> 
  inner_join( geolocalizacao_escolas, by = c("Código da Escola"="Código INEP"))

IDEB_Respostas_ANOS_FINAIS <-IDEB_BA_ENSINO_MEDIO |> 
  inner_join( INSE_GEO_RESPOSTA_SSA, by = c("Código da Escola"="Código INEP"))

GEO_IDEB_Respostas_ENSINO_MEDIO %>%
  drop_na(Latitude, Longitude)%>%
  leaflet() %>%
  addTiles() %>%
  addProviderTiles(providers$OpenStreetMap.DE) %>%
  setView(-38.4368023,-12.9144042,10) %>%
  addCircles(lng=~Longitude,
             lat=~Latitude,
             radius = ~`Indicador de Rendimento (P)`,
             label = ~htmlEscape(`Nome da Escola`))

#IDEB_ENSINO_MEDIO<- full_join(x = IDEB_BA_ANOS_FINAIS,
 #                        y = mapeamento_salvador,
  #                       by = c("Código da Escola"="Código INEP"))






#INSE_GEO_RESPOSTA_SSA %<>% mutate(INSE_VALOR_ABSOLUTO = as.numeric(INSE_VALOR_ABSOLUTO))
#INSE_GEO_RESPOSTA_SSA %<>% drop_na(INSE_VALOR_ABSOLUTO)
#mean(INSE_GEO_RESPOSTA_SSA$INSE_VALOR_ABSOLUTO)





#names(geolocalizacao_escolas)[3] <- c("codigo_inep")
#names(mapeamento_salvador)[18] <- c("codigo_inep")
#names(INSE_2019_ESCOLAS)[1] <- c("codigo_inep")

#mapeamento_salvador_localizacao <- mapeamento_salvador |>
#  inner_join(geolocalizacao_escolas) 
#View(mapeamento_salvador_localizacao)

#mapeamento_salvador_localizacao %>%
#drop_na(Latitude, Longitude)%>%
# leaflet() %>% 
#addTiles() %>% 
#addProviderTiles(providers$OpenStreetMap.DE) %>% 
#setView(-38.4368023,-12.9144042,10) %>%
#addMarkers(lng=~Longitude,
#          lat=~Latitude, popup =~UF)


#escolas_desempenho <- mapeamento_salvador |>
#  inner_join(INSE_2019_ESCOLAS) 
#View(escolas_desempenho)
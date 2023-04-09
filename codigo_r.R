library(readxl)


#FONTE: https://www.gov.br/inep/pt-br/acesso-a-informacao/dados-abertos/inep-data/catalogo-de-escolas
geolocalizacao_escolas <- read_excel("data/Análise - Tabela da lista das escolas - Detalhado.xlsx", 
                                                                 col_types = c("text", "text", "numeric", 
                                                                                         "text", "text", "text", "text", "text", 
                                                                                         "text", "text", "text", "text", "text", 
                                                                                         "text", "text", "text", "text", "numeric", 
                                                                                         "numeric"))


#FONTE:  https://www.gov.br/inep/pt-br/acesso-a-informacao/dados-abertos/indicadores-educacionais/nivel-socioeconomico
INSE_2019_ESCOLAS <- read_excel("data/INSE_2019_ESCOLAS.xlsx", 
                                     col_types = c("text", "text", "numeric", 
                                                             "text", "numeric", "text", "numeric", 
                                                             "numeric", "numeric", "numeric", 
                                                             "numeric", "text", "numeric", "numeric", 
                                                             "numeric", "numeric", "numeric", 
                                                             "numeric", "numeric", "numeric"))

mapeamento_salvador <- read_excel("data/mapeamento_clube_ciencia_salvador_regiao_metropolitana.xlsx", 
                                                                          col_types = c("date", "text", "text", 
                                                                                                  "text", "text", "text", "text", "text", 
                                                                                                  "text", "text", "text", "text", "text", 
                                                                                                  "text", "text", "text", "text", "numeric"))


#install.packages("leaflet")
#install.packages("leaflet.extras")
library(dplyr)
library(leaflet)
library(leaflet.extras)
library(tidyr)



geolocalizacao_escolas %>%
  drop_na(Latitude, Longitude)%>%
  leaflet() %>% 
  addTiles() %>% 
  addProviderTiles(providers$OpenStreetMap.DE) %>% 
  setView(-38.4368023,-12.9144042,10) %>%
  addMarkers(lng=~Longitude,
             lat=~Latitude, popup =~UF)







## Versão atualizada em abril de 2020
#Observações:
## Baseado em: https://dataficacao.wordpress.com/2017/02/21/criando-mapa-brasil-r/
#O "shapefile" do Brasil no site do IBGE estão no endereço: http://downloads.ibge.gov.br/downloads_geociencias.htm
# Shapefile = formato popular de arquivo contendo dados geoespaciais em forma de vetor utilizados nos GIS/SIG
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#1. Instalando os pacotes----
#
# Ao rodar programa, deve-se verificar se os pacotes necessários estão instalados no RStudio
if (!"rgeos" %in% installed.packages()) install.packages("rgeos")
if (!"maptools" %in% installed.packages()) install.packages("maptools")
if (!"spdep" %in% installed.packages()) install.packages("spdep")
if (!"cartography" %in% installed.packages()) install.packages("cartography")
if (!"tmap" %in% installed.packages()) install.packages("tmap")
if (!"leaflet" %in% installed.packages()) install.packages("leaflet")
if (!"dplyr" %in% installed.packages()) install.packages("dplyr")
if (!"rgdal" %in% installed.packages()) install.packages("rgdal")
if (!"RColorBrewer" %in% installed.packages()) install.packages("RColorBrewer")

# ...e carregando os pacotes----
library(rgeos)
library(maptools)     
library(spdep)          
library(cartography)    
library(tmap)           
library(leaflet)        
library(dplyr)
library(rgdal)
library(RColorBrewer) 

#2. Importando shapefile (mapa do Brasil)----
dsn <- "./dados/" # Indicando o local dos arquivos shapefiles
list.files(dsn, pattern='\\.shp$') # mostrando a lista de shapefiles no diretório
shp <- readOGR(dsn="./dados", layer="BRUFE250GC_SIR", stringsAsFactors=FALSE, encoding="UTF-8") # lendo os shapefiles
class(shp) # verificando o tipo de arquivo

#3. Importando dataset com os dados a serem plotados no mapa----
#O dataset “ClassificacaoPontosCorridos.csv” tem os dados dos pontos ganhos por cada clube 
#no Campeonato Brasileiro desde 2003
#Importação do arquivo
pg <- read.csv(paste0(dsn, "ClassificacaoPontosCorridos.csv"), header=T,sep=";", encoding="UTF-8")
##Sumarização dos pontos ganhos por estado, utilizando as funções do pacote dplyr
# Nota: O operador de atribuição composta %>% é usado para atualizar um valor, canalizando-o primeiro em uma ou mais expressões e depois atribuindo o resultado.
pg <- pg %>% group_by(Estado) %>% mutate(cumsum = cumsum(PG))
pg <- pg %>%
  group_by(Estado) %>%
  summarise(Score= max(cumsum))
pg <- as.data.frame(pg)
class(pg)

#4. Importando códigos do IBGE e adicionr ao dataset o campo "UF" ----
ibge <- read.csv(paste0(dsn, "estadosibge.csv"), header=T,sep=",",encoding="UTF-8")
pg <- merge(pg,ibge, by.x = "Estado", by.y = "UF")
# Fazendo a junção entre o dataset e o shapefile----
pg <- merge(shp,pg, by.x = "CD_GEOCUF", by.y = "Código.UF")

#5. Fazer a junção entre o dataset e o shapefile utilizando o código do IBGE
brasileiropg <- merge(shp,pg, by.x = "CD_GEOCUF", by.y = "Código.UF")

#6. Realizando o tratamento e a formatação do data frame espacial
proj4string(brasileiropg) <- CRS("+proj=longlat +datum=WGS84 +no_defs")
Encoding(brasileiropg$NM_ESTADO) <- "UTF-8"
brasileiropg$Score[is.na(brasileiropg$Score)] <- 0

#7. Gerando o mapa
pal <- colorBin("Blues",domain = NULL,n=5) #cores do mapa

state_popup <- paste0("<strong>Estado: </strong>", 
                      brasileiropg$NM_ESTADO, 
                      "<br><strong>Pontos: </strong>", 
                      brasileiropg$Score)
leaflet(data = brasileiropg) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(fillColor = ~pal(brasileiropg$Score), 
              fillOpacity = 0.8, 
              color = "#BDBDC3", 
              weight = 1, 
              popup = state_popup) %>%
  addLegend("bottomright", pal = pal, values = ~brasileiropg$Score,
            title = "Pontos Conquistados",
            opacity = 1)

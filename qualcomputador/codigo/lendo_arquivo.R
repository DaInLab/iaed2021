# Versão melhorada de novembro de 2021
#lendo no formato xlsx
if(!"readxl" %in% installed.packages()) install.packages("readxl")
library(readxl)
computador_xlsx <- read_excel("./dados/qual_computador.xlsx")
View(computador_xlsx)
class(computador_xlsx)
#[1] "tbl_df"     "tbl"        "data.frame"

#lendo arquivo no formato texto!
computador_csv <- read.csv("./dados/qual_computador.csv")
View(computador_csv)
class(computador_csv)
#[1] "data.frame"

#Lendo arquivo no formato json!
if(!"rjson" %in% installed.packages()) install.packages("rjson")
library("rjson")
json_file <- "./dados/qual_computador.json"
json_data <- fromJSON(file=json_file)
#se verificar o tipo de conversão realizada, ficou tipo "lista"
class(json_data)
#[1] "list"
View(json_data)

#para transformar em data.frame
if(!"plyr" %in% installed.packages()) install.packages("plyr")
library(plyr)
df_seu_laptop.1 <- data.frame(t(sapply(json_data,c)), stringsAsFactors = F)
View(df_seu_laptop.1)
class(df_seu_laptop.1)
#[1] "data.frame"

#outra biblioteca para ler arquivos tipo json
if(!"jsonlite" %in% installed.packages()) install.packages("jsonlite")
library(jsonlite)
json_data_frame <- fromJSON(json_file, flatten = T)
class(json_data_frame)
#[1] "array"

df <- as.data.frame(json_data_frame, make.names = F, 
                    stringsAsFactors = F)
View(df)
class(df)
#[1] "data.frame"

#lendo tablemas em formato HTML
if(!"htmltab" %in% installed.packages()) install.packages("htmltab")
library(htmltab)
html_file <- "./dados/Sobre seu laptop.html"
computador.html <- htmltab(doc = html_file, which = 1)
View(computador.html)
class(computador.html)
#[1] "data.frame"
## Um exemplo "pegando" uma tabela de dados sobre população mundial
##no site Wikipedia na url citada no comando url = 
url <- "http://en.wikipedia.org/wiki/World_population"
xp <- "//caption[starts-with(text(),'World historical')]/ancestor::table"
tabela.populacao.mundial = htmltab(doc = url, which = xp)
View(tabela.populacao.mundial)
##lendo arquivo no formato .ods (Calc)
##O Open Document Format for Office Applications (ODF), também conhecido como OpenDocument, 
##é um formato de arquivo baseado em XML compactado em ZIP para planilhas, gráficos, 
##apresentações e documentos de processamento de texto. Foi desenvolvido com o objetivo 
##de fornecer uma especificação aberta de formato de arquivo baseado em XML para aplicativos 
##de escritório.
if(!"readODS" %in% installed.packages()) install.packages("readODS")
library(readODS)
file_ods = "./dados/qual_computador.ods"
computador.ods = read_ods(path = file_ods, sheet = 1, col_names = TRUE,
         col_types = NULL, na = "", skip = 0, formula_as_formula = FALSE,
         range = NULL)
View(computador.ods)
class(computador.ods)
#[1] "data.frame"

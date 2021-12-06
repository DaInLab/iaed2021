
if(!("devtools") %in% installed.packages()) install.packages("devtools")
library(devtools)

if(!("installr") %in% installed.packages()) install.packages("installr")
library(installr)

#Instalar "RTools por fora" - baixar do endereço: https://cran.rstudio.com/bin/windows/Rtools/
# depois gerar o arquivo.Renviro através do comando:
writeLines('PATH="${RTOOLS40_HOME}\\usr\\bin;${PATH}"', con = "~/.Renviron")
#Fazer isso apenas uma vez !!
#
# Importante: Após "rodar" apenas uma vez, é necessário reinciar o RStudio !
#
#Testar se a instalação do Rtools é "vista" pelo R (ou RStudio)
Sys.which("make")
# O resultado pode ser:
#                              make 
#"C:\\rtools40\\usr\\bin\\make.exe"

#Para testar se está tudo ok!
#Try to install an R package from source:

install.packages("jsonlite", type = "source")
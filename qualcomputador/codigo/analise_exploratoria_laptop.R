# Versão 3 de novembro de 2021
# No R, para utilizar determinadas funções, que não estão no pacote "base" é preciso "carregar" a biblioteca no programa
# O comando que realiza esta função é chamado de "library"
# Neste caso, utilizaremos o "pacote" readxl diretamente do repositório existente no CRAN-R (oficial)
# Caso o mesmo ainda não esteja transferido o comando é instalado pelo comando abaixo:
if(!"readxl" %in% installed.packages()) install.packages("readxl")
library(readxl)
#Lendo a planilha e carregando no R como "data frame". 
#O data frame é usado para armazenar as tabelas de dados em R. 
#É uma tabela ou uma estrutura bidimensional do tipo matriz em que cada coluna contém os valores de uma variável
#e cada linha contém um conjunto de valores de cada coluna.
#É uma lista de vetores de igual comprimento

meu.dataframe <-read_excel("./dados/qual_computador_tratado.xlsx")

head(meu.dataframe, 2)

# Alguns gráficos básicos
# Tipo pizza
pie(meu.dataframe$so)

# Para melhorar a visualização, usaremos a função table() para subdividir o gráfico em fatores
# table() usa os fatores de classificação cruzada (classificação de acordo com mais de um atributo ao mesmo tempo)
# para construir uma tabela de contingência (tabela estatística que mostra as freqüências dos dados, 
# classificados de acordo com duas variáveis: as linhas indicam uma variável e as colunas indicam outra variável)
# das contagens em cada combinação de níveis de fatores.
table(meu.dataframe$so)
# 0  1  3  4 (fatores, onde 0 = Não sei, 1 = Windows, 2 = MacOs e 3 = Linux, 4 = Outro)
# 1 12  3  1 
pie(table(meu.dataframe$so))
#Incluindo rótulos (label) no gráfico
lbls <- c("Não sei !", "Windows", "Linux", "Outro")
pct <- round(table(meu.dataframe$so)/sum(table(meu.dataframe$so))*100, digits=1)
pct
#  0    1    3    4 
#5.9 70.6 17.6  5.9 
lbls <- paste(lbls, pct) # adicionar os valores das percentagens nos labels 
lbls <- paste(lbls,"%",sep="") # adicionar o símbolo % aos labels 
pie(table(meu.dataframe$so), labels = lbls, edges = 200, radius = 0.8,
    clockwise = TRUE, density = 60, angle = 45, col = c("purple", "red", "green3","cyan"), border = NULL,
    lty = NULL, main = "Sistemas Operacionais Utilizados pelos Alunos")

# Quanto ao gênero
pct <- round(table(meu.dataframe$genero)/sum(table(meu.dataframe$genero))*100, digits=1)
pct
#    1    2 (fatores, onde 1 = Feminino, 2 = Masculino e 3 = Não Declarado)
# 23.5 76.5 
lbls <- c("Feminino", "Masculino", "Não Declarado")
lbls <- paste(lbls, pct) # adicionar os valores das percentagens nos labels 
lbls <- paste(lbls,"%",sep="") # adicionar o símbolo % aos labels 
pie(table(meu.dataframe$genero), labels = lbls, edges = 200, radius = 0.8,
    clockwise = TRUE, density = 60, angle = 45, col = c("purple", "green3","cyan"), border = NULL,
    lty = NULL, main = "Gênero dos Alunos")

# Quanto à faixa etária
pct <- round(table(meu.dataframe$idade)/sum(table(meu.dataframe$idade))*100, digits=1)
pct
#    1    2    3    5 # oito tipos de faixa etária! Neste questionário apenas 4 estão presente
# 23.5 35.3 35.3  5.9
#lbls <- c("18/21 anos","22/24 anos","25/29 anos",
#          "30/34 anos","35/39 anos","40/49 anos",
#          "45/54 anos","50/59 anos","mais de 59 anos")

lbls <- c("18/21 anos","22/24 anos","25/29 anos","35/39 anos")

lbls <- paste(lbls, pct) # adicionar os valores das percentagens nos labels 
lbls <- paste(lbls,"%",sep="") # adicionar o símbolo % aos labels 
pie(table(meu.dataframe$idade), labels = lbls, edges = 200, radius = 0.8,
    clockwise = TRUE, density = 60, angle = 45, col = c("red","purple", "green3","yellow"), border = NULL,
    lty = NULL, main = "Idade dos Alunos")

# Quanto ao fabricante/marca do computador
pct = table(meu.dataframe$marca)
pct
#  Acer    Dell  HP  Lenovo LG  Não possuo  Samsung 
#             5   1       3  1           1        4
class(pct) # Os objetos R têm um atributo de classe, um vetor que fornece os nomes das classes nas quais o objeto é herdado.
#[1] "table"
# A variável pct é do tipo "table" (tabela)
# Para obter os "nomes" dos itens da tabela (se houver!) pode-se utilizar a função "names"
names(pct) # Função para obter ou definir os nomes em um objeto.
#[1] "Acer"    "Apple"   "Dell"    "HP"      "Lenovo"  "Samsung"
# Atribuindo à variável lbls os nomes da tabela
lbls = names(pct)
# Calculando a porcentagem
pct = round(table(meu.dataframe$marca)/sum(table(meu.dataframe$marca))*100, digits=1)
lbls <- paste(lbls, pct) # adicionar os valores das percentagens nos labels 
lbls <- paste(lbls,"%",sep="") # adicionar o símbolo % aos labels 

#Desenhando o gráfico
pie(table(meu.dataframe$marca), labels = lbls, edges = 200, radius = 0.8,
    clockwise = F, density = 60, angle = 45, 
    col = c("red", "yellow",  "purple","green3","cyan"),
    border = NULL,
    lty = NULL, main = "Marca dos Laptops")

# Quanto ao curso
pct = table(meu.dataframe$curso)
pct
# Ciência da computação Sistemas de Informação 
#                     6                     11
names(pct)
# [1] "Ciência da computação"  "Sistemas de Informação"

lbls = names(pct)

# Calculando a porcentagem
pct = round(table(meu.dataframe$curso)/sum(table(meu.dataframe$curso))*100, digits=1)
lbls <- paste(lbls, pct) # adicionar os valores das percentagens nos labels 
lbls <- paste(lbls,"%",sep="") # adicionar o símbolo % aos labels 

#Desenhando o gráfico de cursos
pie(table(meu.dataframe$curso), labels = lbls, edges = 200, radius = 0.8,
    clockwise = TRUE, density = 60, angle = 45, 
    col = c("red", "yellow",  "purple","green3","cyan"),
    border = NULL,
    lty = NULL, main = "Cursos dos Alunos")


# Título: "Um Projeto de Análise Exploratória de Dados em R"
# Autora: "Rashida Nasrin Sucky"
# Data: "22 dez. 2020"
# url: {https://towardsdatascience.com/an-exploratory-data-analysis-project-in-r-a51918ebf34d}
# Versão 1.2 em português: 08 jan. 2022.
# Objetivo: Resumir e visualizar os dados para uma melhor compreensão, 
#            mesmo que todas as variáveis não sejam compreensíveis
# -----------------------------------------------------------------------------------------

# O conjunto de dados utilizado é uma amostra sobre doenças cardíacas. 
# Cada linha representa os diferentes dados de saúde de uma pessoa.
# O dataset foi baixado da plataforma Kaggle (http://kaggle.com/johnsmith88/heart-disease-dataset?select=heart.csv) 
# e colocado na pasta "data" do projeto.

# Importando o conjunto de dados no ambiente RStudio 
heart = read.csv("./data/heart.csv")

# Preparação de dados
# Inicialmente iremos verificar a correlação entre as doenças cardíacas e as outras variáveis no conjunto de dados.
# Será utilizada a biblioteca ‘corrplot’ para gerar um gráfico que mostrará a correlação de cada variável com as outras.
if(!("corrplot") %in% installed.packages()) install.packages("corrplot")
library(corrplot)
# Armazenando os gráficos resultantes, com formato jpeg, no diretório 'graphics'
jpeg(file = "./graphics/correlacao.jpeg")
corrplot(cor(heart), type="upper")
dev.off()

# O gráfico de correlação mostra que os parâmetros ‘restecg’, ‘fbs’ e ‘chol’ estão vagamente correlacionados 
# com a ‘variável alvo. 
# Portanto, podemos excluí-las com segurança do conjunto de dados para este estudo específico.
# Excluindo as váriáveis ‘restecg’, ‘fbs’ e ‘chol’
heart = subset(heart, select=c(-restecg, -chol,-fbs))

# Gerando novamente o gráfico de correlação
jpeg(file = "./graphics/correlacao_11.jpeg")
corrplot(cor(heart), type="upper")
dev.off()

# O foco nesta AED é o de descobrir a relação entre a doença cardíaca e outros parâmetros, 
# observaremos a correlação entre a variável target ("alvo") com as outras variáveis. 
# O tamanho dos pontos no gráfico mostra o quão forte é a correlação.

# Demonstraremos a análise de algumas relações entre as variáveis discretas e algumas categóricas com a variável "alvo".
# As variáveis categóricas são denotadas como 0, 1, 2, 3. 
# Foram alteradas para valores de string mais significativos de acordo com a descrição acima.

heart$sex[heart$sex == 0] = "female"
heart$sex[heart$sex == 1] = "male"

heart$cp[heart$cp == 0] = "typical angina"
heart$cp[heart$cp == 1] = "atypical angina"
heart$cp[heart$cp == 2] = "non-anginal pain"
heart$cp[heart$cp == 3] = "asymptomatic"

heart$exang[heart$exang == 0] = "no"
heart$exang[heart$exang == 1] = "yes"

heart$slope[heart$slope == 0] = "upsloping"
heart$slope[heart$slope == 1] = "flat"
heart$slope[heart$slope == 2] = "downsloping"

heart$thal[heart$thal == 1] = "normal"
heart$thal[heart$thal == 2] = "fixed defect"
heart$thal[heart$thal == 3] = "reversible defect"

heart$target1 = heart$target
heart$target1[heart$target1 == 0] = "no heart disease"
heart$target1[heart$target1 == 1] = "heart disease"

# O conjunto de dados agora está pronto ! 
# Vamos agora mergulhar na análise exploratória.

# Análise Exploratória de Dados (EAD)
# A análise exploratória de dados começa à partir das perguntas, curiosidades e necessidades.

# É intuitivo começar com a proporção de pessoas "com doenças cardíacas" e "sem doenças cardíacas".

round(prop.table(table(heart$target1)),2)
#  heart disease no heart disease 
#           0.51             0.49 

# Idade da População
# Osenso comum diz que os idosos são mais propensos a apresentar doenças cardíacas. 
# No gráfico a seguir mostra-se a distribuição da idade da população no conjunto de dados.
library(ggplot2)

jpeg(file = "./graphics/idade_pop.jpeg")
ggplot(heart, aes(x=age)) +
  geom_histogram() + ggtitle("Distribuição da idade da população")+
  xlab("Idade") + ylab("Densidade")
dev.off()

# A distribuição é quase normal e ligeiramente enviesada para a direita. 
# A maioria da população encontra-se na faixa etária de 50 a 65 anos. 
# Muito poucas pessoas estão na faixa dos trinta e muito poucas pessoas na faixa dos 70 anos.

# Olhar para a faixa etária pode ser mais significativo em termos de taxa de doenças cardíacas.

# a variável age ('idade') foi dividida para formar diferentes grupos de idade 
# e uma coluna (variável) foi criada com o nome 'idade_grp'
heart$age_grp = cut(heart$age, breaks = seq(25, 77, 4))

# Poderemos descobrir o número de pessoas com doenças cardíacas para cada faixa etária
library(tidyverse)

target_by_age = heart %>%
  group_by(age_grp) %>%
  summarise(heart_disease = sum(target))
target_by_age

# A tibble: 12 × 2
#  age_grp heart_disease
#   <fct>           <int>
# 1 (25,29]             4
# 2 (33,37]            20
# 3 (37,41]            50
# 4 (41,45]            82
# 5 (45,49]            43
# 6 (49,53]            87
# 7 (53,57]            80
# 8 (57,61]            52
# 9 (61,65]            53
#10 (65,69]            35
#11 (69,73]            14
#12 (73,77]             6

# A tabelao mostra a faixa etária das pessoas e a quantidade de pessoas cardíacas em cada grupo.
# Para descobrir a frequência de pessoas com doenças cardíacas em cada faixa etária, 
# o gráfico de barras a seguir mostra a distribuição com esses dados.
jpeg(file = "./graphics/pessoas_card_faixa.jpeg")
target_by_age %>%
  ggplot(aes(x=age_grp, y=heart_disease)) +
  geom_bar(stat="identity", fill="#f68060", alpha=.6, width=.4) +
  xlab("") + ylab("Qtde. de Pessoas com doença cardíaca") + ggtitle("Número de Pessoas com doença cardíaca por Faixa Etária") + 
  theme_bw()
dev.off()

# O gráfico mostra que a faixa etária entre 49 e 57 anos possui o maior número de pessoas com doença cardíaca.

# Para visualizar melhor este fenômeno, vamos calcular a proporção de pacientes com doenças cardíacas em cada faixa etária.
prop_in_age = heart %>%
  group_by(age_grp) %>%
  summarise(heart_disease_proportion = round(sum(target)/n(), 3)*100)
prop_in_age

# A tibble: 12 × 2
#   age_grp heart_disease_proportion
#   <fct>                      <dbl>
# 1 (25,29]                    100  
# 2 (33,37]                     74.1
# 3 (37,41]                     72.5
# 4 (41,45]                     72.6
# 5 (45,49]                     53.1
# 6 (49,53]                     67.4
# 7 (53,57]                     44.7
# 8 (57,61]                     28.6
# 9 (61,65]                     40.8
#10 (65,69]                     45.5
#11 (69,73]                     56  
#12 (73,77]                     66.7

# O gráfico de barras correspondente aos dados
jpeg(file = "./graphics/card_faixa_etaria.jpeg")
prop_in_age %>%
  ggplot(aes(x=age_grp, y=heart_disease_proportion)) +
  geom_bar(stat="identity", fill="#f68060", alpha=.6, width=.4) +
  xlab("Faixa Etária") + ylab("Pessoas com Doença Cardíaca") + ggtitle("Proporção de Doença Cardíaca por Faixa Etária") + 
  theme_bw()
dev.off()

# Abaixo de 30 anos, 100% das pessoas têm doenças cardíacas! Definitivamente, esse não é o caso na vida real. 
# Esta não é uma amostra representativa.

# Gênero ou sexo
# Para examinar a relação entre doenças cardíacas e gênero, é importante verificar 
# a proporção de homens e mulheres neste conjunto de dados.

round(prop.table(table(heart$sex)),2)
#female   male 
#   0.3    0.7

# 30% da amostra é do gênero feminino e 70%, masculino. 
# A proporção de homens e mulheres com doenças cardíacas pode ser a próxima descoberta importante.
round(prop.table(table(heart$sex, heart$target1)), 2)
#         heart disease no heart disease
#  female          0.22             0.08
#  male            0.29             0.40
# Pessoas com doenças cardíacas são muito mais numerosas na população feminina.
# Na população masculina, 40% não têm doenças cardíacas e apenas 29% têm doenças cardíacas.

# Variação do segmento ST do exercício de pico (slope)
#Um gráfico de barras dos diferentes tipos de variação (elevação ou descenso) e condições 
# de doenças cardíacas poderá ser mais adequado para enrendê-lo. 
jpeg(file = "./graphics/tipo_slope_ST.jpeg")
ggplot(heart, aes(x= slope, fill=target1)) + 
  geom_bar(position = 'dodge') +
  xlab("Tipo de Variação") +
  ylab("Qtde") +
  ggtitle("Análise do tipo de Variação") +
  scale_fill_discrete(name = "Doença Cardíaca", labels = c("Não", "Sim"))
dev.off()

# Claramente, com diferentes tipos de inclinação, a taxa de doenças cardíacas parece diferente. 
# Com declive (downsloping), o número de pessoas sem doenças cardíacas é muito maior (cerca de 340) 
# do que o número de pacientes com doenças cardíacas (cerca de 125). 
# Com variação plana (flat) é quase o oposto. O número de cardiopatias é de cerca de 325 
# e o número de não cardiopatas é de cerca de 160. 
# Na tendência ascendente (upsloping), não há muitas diferenças, mas o número de cardiopatias 
# é maior do que o número de casos sem cardiopatia.

# Pergunta importante: Essa tendência é a mesma na população masculina e feminina?

# O conjunto de dados será separado em dois grupos: 
male_data = heart[heart$sex=="male",]

female_data = heart[heart$sex=="female",]

# Utilizaremos o mesmo tipo de gráfico de barras para analisar a população masculina e a feminina.
jpeg(file = "./graphics/tipo_slope_ST_masculino.jpeg")
ggplot(male_data, aes(x= slope, fill=target1)) + 
  geom_bar(position = 'dodge') +
  xlab("Tipo de Variação") +
  ylab("Qtde") +
  ggtitle("Análise dos tipos de variação para homens") +
  scale_fill_discrete(name = "Doença Cardíaca", labels = c("Não", "Sim"))
dev.off()

jpeg(file = "./graphics/tipo_slope_ST_feminino.jpeg")
ggplot(female_data, aes(x= slope, fill=target1)) + 
  geom_bar(position = 'dodge') +
  xlab("Tipo de Variação") +
  ylab("Qtde") +
  ggtitle("Análise dos tipos de variação para mulheres") +
  scale_fill_discrete(name = "Doença Cardíaca", labels = c("Não", "Sim"))
dev.off()

# O gráfico da população masculina segue a mesma tendência do gráfico de barra geral para análise de declive (downsloping). 
# Na população feminina o número decrescente de nenhuma doença cardíaca é muito maior (180) do que o número de doenças cardíacas (25). 
# Para a variação plana, ambos os casos são próximos, mas o número de casos sem doença cardíaca é um pouco maior.

# Número de principais vasos (ca)
#O conjunto de dados mostra que pode haver 0, 1, 2, 3 ou 4 vasos principais do coração em uma pessoa. 
#De acordo com o gráfico de correlação, o número de vasos tem uma boa correlação com as doenças cardíacas. 
# A representação visual de quão diferente o número de vasos principais se relaciona com as doenças cardíacas
# é mostrada no gráfico a seguir:

jpeg(file = "./graphics/vasos_principais.jpeg")
mosaicplot(table(heart$target1, heart$ca), 
           col=c("#754869", "coral", "skyblue", "#423f42", "#ed18c6"), 
           las=1, 
           main="Cardiopatias para os Vasos Principais")
dev.off()

# 2/3 das pessoas com doença cardíaca não apresentam nenhum vaso importante. 
# Poucas pessoas têm 4 vasos principais. Portanto, é difícil saber o impacto disso.

# As populações masculinas e femininas podem ter um número diferente de vasos principais 
# ou níveis diferentes de correlação entre os vasos principais e as doenças cardíacas. 

# O gráfico mostra os principais vasos vs doenças cardíacas em homens: 

jpeg(file = "./graphics/vasos_principais_homens.jpeg")
mosaicplot(table(male_data$target1, male_data$ca),
           col=c("#754869", "coral", "skyblue", "#423f42", "#ed18c6"), 
           las=1, 
           main="Vasos Principais em Homens")
dev.off()
# O gráfico masculino da amostra parece seguir uma tendência semelhante à da população total.

# Gráfico da correlação do número de vasos principais e doenças cardíacas na população feminina: 
jpeg(file = "./graphics/vasos_principais_mulheres.jpeg")  
mosaicplot(table(female_data$target1, female_data$ca), 
           col=c("#754869", "coral", "skyblue", "#423f42", "#ed18c6"), 
           las=1, 
           main="Vasos Principais em Mulheres")
dev.off()

# Depressão de ST induzida por exercício em relação ao repouso (oldpeak)
# Os boxplots a seguir mostram a distribuição da depressão do segmento ST para pessoas com doenças cardíacas e sem doenças cardíacas. 
jpeg(file = "./graphics/st_oldpeak.jpeg")
ggplot(heart, aes(x = target1, y = oldpeak)) + 
  ylab("Depressão ST") + xlab("Estado da Doença Cardíaca")+ 
  ggtitle("Depressão de ST induzida por Exercício vs Cardiopatia")+
  geom_boxplot()
dev.off()
# No lado sem doenças cardíacas, o intervalo interquartil é maior (cerca de 2) do que no lado das doenças cardíacas (1).

# Será que esse tipo de depressão muda com a idade e, juntos, eles têm impactos diferentes nas doenças cardíacas?
# Um gráfico de dispersão combinado pode fornecer alguns insights sobre isso. 
jpeg(file = "./graphics/st_oldpeak_combinado.jpeg")
ggplot(heart, aes(x = age, y = oldpeak,color=target1, size = factor(oldpeak))) + 
  geom_point(alpha=0.3) + labs(color = "Estado da doença cardíaca")+guides(size=FALSE) + xlab("Idade") + ylab("Depressão ST") + ggtitle("Idade versus Pressão Arterial em repouso separada por Condição Cardíaca ")
dev.off()
# Este conjunto de dados é diferente. Ele mostra as doenças cardíacas diminuem quanto mais elevada a idade. 
# A partir do gráfico ficou difícil derivar qualquer relação entre idade e depressão de ST.

# Pressão sanguínea em repouso
# Os boxplots de açúcares no sangue em repouso separados por estado de doença cardíaca poderão fornecer uma ideia inicial, 
# como mostra o gráfico a seguir. 
jpeg(file = "./graphics/pressao_sanguinea.jpeg")
ggplot(heart, aes(x = target1, y = trestbps)) +
  geom_boxplot() + xlab("Estado da Doença Cardíaca ") + 
  ylab("Pressão sanguínea em repouso") + 
  ggtitle("Boxplots de pressão arterial em repouso por Condição Cardíaca")
dev.off()

# O próximo gráfico é um gráfico de dispersão de idade versus pressão arterial em repouso,
# que inclui **cores diferentes para o estado de doença cardíaca e o tamanho do ponto depende da depressão St**. 
# Este gráfico deve revelar mais algumas informações.
jpeg(file = "./graphics/pressao_sanguinea_cores.jpeg")
ggplot(data=heart,aes(x=age,y=trestbps,color=target1,size=factor(oldpeak)))+
  geom_point(alpha=0.3)+
  xlab("Idade")+
  ylab("Açúcar no sangue em repouso ") + 
  labs(color="Estado da doença cardíaca ") + 
  guides(size=FALSE)+
  ggtitle("Idade vs Pressão Arterial em repouso vs Condição Cardíaca")
dev.off()





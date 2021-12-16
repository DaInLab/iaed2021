# Fonte: https://dataficacao.wordpress.com/2017/01/05/text-mining-r-exemplo-pratico-dados-twitter/

install.packages("twitteR")
install.packages("ROAuth")
install.packages("SnowballC")
install.packages("tm")


library("twitteR")
library("ROAuth")
library(SnowballC)
library(tm)
library(stringr)
library(stringi)

#Obter Dados do Twitter----


download.file(url="http://curl.haxx.se/ca/cacert.pem",destfile="cacert.pem")

# create an object "cred" that will save the authenticated 
# object that we can use for later sessions
# input your own consumerKey and Secret below
cred <- OAuthFactory$new(consumerKey='xxxxxxxxxxxxxxxxxx', 
                         consumerSecret='xxxxxxxxxxxxxxxxxxxxxxxxxxxx',
                         requestURL='https://api.twitter.com/oauth/request_token',
                         accessURL='https://api.twitter.com/oauth/access_token',
                         authURL='https://api.twitter.com/oauth/authorize')

# Executing the next step generates an output
# To enable the connection, please direct your web  
# browser to: <hyperlink>. 
# Note: You only need to do this part once
cred$handshake(cainfo="cacert.pem")

#save for later use for Windows
save(cred, file="twitter_authentication.Rdata")

# Load "twitter authentication.Rdata" file in your session 
# and then run registerTwitterOAuth. 
# This should return "TRUE" indicating that all is good 
# and we can proceed. 
load("twitter_authentication.Rdata")

api_key = 'xxxxxxxxxxxxxxxxxxxxxxxxxxx'
api_secret = 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
access_token = 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
access_secret = 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx'

setup_twitter_oauth(api_key,api_secret,access_token,access_secret)

#Consulta aos tweets com a hashtag "batebola"----

search.string <- "#bateboladebate"
no.of.tweets <- 1106

batebola <- searchTwitter(search.string, no.of.tweets,since = "2016-12-12",
                          until = "2016-12-13",
                          lang="pt")


#saveRDS(batebola,"batebola.rds") #salvando dados em formato RDS

#Lendo tweets e realizando as transformações----

#batebola <- readRDS("batebola.rds")

class(batebola)
summary(batebola)

batebola[1023] #exemplo: tweet 1023

dados <- twListToDF(batebola) #convertendo lista em data frame
dados_str <- stri_trans_general(dados$text,'Latin-ASCII') #removendo acentuação das palavras

dados_str[1023] #tweet 1023 após a remoção do acento

class(dados)
class(corpus_text)

dados_transf <- as.data.frame(dados_str)

class(dados_transf)

dados_transf <- as.data.frame(dados_str)
corpus <- Corpus(VectorSource(dados_str))

corpus <- tm_map(corpus, stripWhitespace) #realizando tratamento de múltiplos espaços
corpus <- tm_map(corpus, tolower) #transformando em letra minúsucula
corpus <- tm_map(corpus, function(x) gsub('@[[:alnum:]]*', '', x)) #removendo menções
corpus <- tm_map(corpus, removePunctuation) #removendo pontuação
corpus <- tm_map(corpus, removeNumbers) #removendo números
corpus <- tm_map(corpus, function(x) gsub('http[[:alnum:]]*', '', x))  #removendo URLs
corpus <- tm_map(corpus, removeWords, c(stopwords('portuguese'), 'bateboladebate','rt')) #removendo "stopwords"

corpus_text <- tm_map(corpus, PlainTextDocument)

class(corpus_text)

print(corpus_text[[1023]]$content) #tweet 1023 após todas as transformações

#Create Term Document Matrix
tdm <- TermDocumentMatrix(corpus_text, control=list(minWordLength=1))
term.freq <- rowSums(as.matrix(tdm))
term.freq <- subset(term.freq, term.freq >=20)

df <- data.frame(term = names(term.freq), freq = term.freq)


View(df[order(-df$freq),])


#Validando a ocorrência das palavras com relação aos tweets originais

diegosouza<-grepl(pattern="diego souza",x=dados$text,ignore.case=TRUE)
#verificando a quantidade de vezes que a palavra foi mencionada
sum(diegosouza)

gjesus<-grepl(pattern="g.jesus",x=dados$text,ignore.case=TRUE)
#verificando a quantidade de vezes que a palavra foi mencionada
sum(gjesus)

tcheche<-grepl(pattern="tchê tchê",x=dados$text,ignore.case=TRUE)
#verificando a quantidade de vezes que a palavra foi mencionada
sum(tcheche)



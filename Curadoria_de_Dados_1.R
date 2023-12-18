#Verificar pasta do diretório
install.packages("vegan")
library(vegan)
install.packages("ggplot2")
library(ggplot2)

aves <- read.csv("AVES.csv", sep=';', stringsAsFactors = TRUE) #Abrirndo e criando o dataframe
nrow(aves) #para saber número de linhas
ncol(aves) #para saber número de colunas
colnames(aves) #para saber nome das colunas

#vemos que o R está considerando algumas colunas vazias como parte do dataframe. Podemos apagá-las 
#para manejar melhor os dados
aves<- aves[,1:38]

colnames(aves) #vamos olhar novamente o nome das colunas

summary(aves) #Avaliando a natureza da informação contida no objeto criado. Testem abrir o arquivo 
#alternando o último arguento da função entre TRUE e FALSE e vejam a diferença. 

####MANIPULANDO E EXPLORANDO DADOS TEXTUAIS E CATEGÓRICOS####


#Vamos avaliar quantas espécies existem na nossa planilha.
levels(aves$NOME.CIENTIFICO)#Para isso, vamos usar a função levels. Ela nos indica a variabilidade de uma informação 
#categórica, ou seja, os diferentes níveis da informação. 


summary(aves$NOME.CIENTIFICO) #essa função nos mostra quantas vezes cada categoria aparece no dataframe, ou 
#seja, a frequência de cada spp

sort(summary(aves$NOME.CIENTIFICO), decreasing = FALSE) #colocando em ordem para avaliar a distribuicao dos valores

colnames(aves)
summary(aves$FAMILIA)
sort(summary(aves$FAMILIA), decreasing = FALSE)

colnames(aves)
summary(aves$SUBFAMILIA)
sort(summary(aves$SUBFAMILIA), decreasing = FALSE)
#avalie quantas famílias e qual a abundância de cada uma no dataframe


#Será que todos esses nomes científicos estão atualmente válidos? Para avaliar isso, vamos usar o pacote rotl que
#é uma interfare para a base de informações do "Open Tree of Life" e oferece diversas funções para 
# manipular dados filogenéticos e taxonômicos.

install.packages("rotl")# pra quem ainda não instalou
library(rotl)


taxa_aves<-tnrs_match_names(aves$NOME.CIENTIFICO, context_name="Birds") #função para checar o nome 
#das spp de acordo com a base do TOF

tnrs_contexts()#para ver todos os contextos taxonômicos que podem ser usados

taxa_aves#vamos olhar o arquivo
colnames(taxa_aves)


taxa_aves[which(taxa_aves$approximate_match==TRUE),]#vamos olhar apenas para as spp que a função encontrou nomes próximos

levels(aves$NOME.CIENTIFICO)# vamos olhar essa info no nosso dataframe

#Assumindo que a plataforma do TOF é confiável pra o grupo taxonômico de interesse, vamos corrigir 
#os nomes científicos no nosso dataframe


aves$NOME.CIENTIFICO<-as.character(aves$NOME.CIENTIFICO) #primeiro precisamos transformar a coluna dos 
#nomes científicos para o tipo "character" e facilitar a manipulação

aves[aves$NOME.CIENTIFICO=="HABIA RUBRA", "NOME.CIENTIFICO"]<- "HABIA RUBICA" #substituindo o nome
#faça o mesmo para a segunda especie indicada como errada
aves[aves$NOME.CIENTIFICO=="XIPHORHYNCHUS GUTTATOIDES", "NOME.CIENTIFICO"]<- "XIPHORHYNCHUS GUTTATUS" #substituindo o nome

aves$NOME.CIENTIFICO<-as.factor(aves$NOME.CIENTIFICO) #retornando a variável para o tipo factor
levels(aves$NOME.CIENTIFICO) #checando se a alteração foi bem sucedida


####Exercício 1
#abra a planilha nomeada peixes
#veja quantas familias foram registradas
#anote a 5 mais frequentes
#veja quantas spp foram registradas
#anote as 5 mais frequentes
#verifique se os nomes científicos estão de acordo com a plataforma TOF
#renomeia as sinonímias
#reavalie quantas sp existem no dataframe
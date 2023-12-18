#Verificar pasta do diretório
install.packages("vegan")
library(vegan)
install.packages("ggplot2")
library(ggplot2)

peixes <- read.csv("peixes.csv", sep=';', stringsAsFactors = TRUE) #data frame e criando o dataframe
nrow(peixes) #para saber número de linhas
ncol(peixes) #para saber número de colunas
colnames(peixes) #para saber nome das colunas

#vemos que o R está considerando algumas colunas vazias como parte do dataframe. Podemos apagá-las 
#para manejar melhor os dados
aves<- aves[,1:38]

colnames(peixes) #vamos olhar novamente o nome das colunas

summary(aves) #Avaliando a natureza da informação contida no objeto criado. Testem abrir o arquivo 
#alternando o último arguento da função entre TRUE e FALSE e vejam a diferença. 

####MANIPULANDO E EXPLORANDO DADOS TEXTUAIS E CATEGÓRICOS####


#Vamos avaliar quantas espécies existem na nossa planilha.
levels(peixes$SPECIES)#Para isso, vamos usar a função levels. Ela nos indica a variabilidade de uma informação 
#categórica, ou seja, os diferentes níveis da informação. 


summary(peixes$SPECIES) #essa função nos mostra quantas vezes cada categoria aparece no dataframe, ou 
#seja, a frequência de cada spp

sort(summary(peixes$SPECIES), decreasing = FALSE) #colocando em ordem para avaliar a distribuicao dos valores

colnames(peixes)
summary(peixes$FAMILY)
sort(summary(peixes$FAMILY), decreasing = FALSE)

colnames(aves)
summary(aves$SUBFAMILIA)
sort(summary(aves$SUBFAMILIA), decreasing = FALSE)
#avalie quantas famílias e qual a abundância de cada uma no dataframe


#Será que todos esses nomes científicos estão atualmente válidos? Para avaliar isso, vamos usar o pacote rotl que
#é uma interfare para a base de informações do "Open Tree of Life" e oferece diversas funções para 
# manipular dados filogenéticos e taxonômicos.

install.packages("rotl")# pra quem ainda não instalou
library(rotl)


taxa_peixes<-tnrs_match_names(peixes$SPECIES, context_name="Vertebrates") #função para checar o nome 
#das spp de acordo com a base do TOF

tnrs_contexts()#para ver todos os contextos taxonômicos que podem ser usados

taxa_peixes#vamos olhar o arquivo
colnames(taxa_aves)


taxa_peixes[which(taxa_peixes$approximate_match==TRUE),]#vamos olhar apenas para as spp que a função encontrou nomes próximos

levels(peixes$SPECIES)# vamos olhar essa info no nosso dataframe

#Assumindo que a plataforma do TOF é confiável pra o grupo taxonômico de interesse, vamos corrigir 
#os nomes científicos no nosso dataframe


peixes$SPECIES<-as.character(peixes$SPECIES) #primeiro precisamos transformar a coluna dos 
#nomes científicos para o tipo "character" e facilitar a manipulação

peixes[peixes$SPECIES=="Astyanax gr. bimaculatus", "SPECIES"]<- "Astyanax bimaculatus" #substituindo o nome
#faça o mesmo para a segunda especie indicada como errada
peixes[peixes$SPECIES=="Erythrinus?erythrinus", "SPECIES"]<- "Erythrinus erythrinus"
peixes[peixes$SPECIES=="Bryconops cf. caudomaculatus", "SPECIES"]<- "Bryconops caudomaculatus"
peixes[peixes$SPECIES=="Crenuchus?spilurus?", "SPECIES"]<- "Crenuchus spilurus"


peixes$SPECIES<-as.factor(peixes$SPECIES) #retornando a variável para o tipo factor
levels(peixes$SPECIES) #checando se a alteração foi bem sucedida


####Exercício 1 - Respostas
#abra a planilha nomeada peixes
#veja quantas familias foram registradas - Foram registradas 29 famílias
#anote a 5 mais frequentes - Characidae – Cichlidae, Lebiasinidae, Erythrinidae, Gymnotidae
#veja quantas spp foram registradas - Foram registradas 148 espécies
#anote as 5 mais frequentes - Hyphessobrycon heterorhabdus, Helogenes marmoratus, Erythrinus erythrinus, Apistogramma gr. Regani, Aequidens tetramerus
#verifique se os nomes científicos estão de acordo com a plataforma TOF
#renomeia as sinonímias - Foram renomeadas as espécies: Astyanax bimaculatus, Erythrinus erythrinus, Bryconops caudomaculatus, Crenuchus spilurus 
#reavalie quantas sp existem no dataframe
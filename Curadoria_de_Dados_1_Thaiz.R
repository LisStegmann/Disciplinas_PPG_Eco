#Verificar pasta do diretório

install.packages("vegan")
library(vegan)
library(ggplot2)

peixes <- read.csv("peixes.csv", sep=';', stringsAsFactors = TRUE) #Abrirndo e criando o dataframe
nrow(peixes) #para saber número de linhas
ncol(peixes) #para saber número de colunas
colnames(peixes) #para saber nome das colunas

#vemos que o R está considerando algumas colunas vazias como parte do dataframe. Podemos apagá-las 
#para manejar melhor os dados
peixes<- peixes[,1:13]

colnames(peixes) #vamos olhar novamente o nome das colunas

summary(peixes) #Avaliando a natureza da informação contida no objeto criado. Testem abrir o arquivo 
#alternando o último arguento da função entre TRUE e FALSE e vejam a diferença. 

####MANIPULANDO E EXPLORANDO DADOS TEXTUAIS E CATEGÓRICOS####


#Vamos avaliar quantas espécies existem na nossa planilha.
levels(peixes$SPECIES)#Para isso, vamos usar a função levels. Ela nos indica a variabilidade de uma informação 
#categórica, ou seja, os diferentes níveis da informação. 


summary(peixes$SPECIES) #essa função nos mostra quantas vezes cada categoria aparece no dataframe, ou 
#seja, a frequência de cada spp

sort(summary(peixes$SPECIES), decreasing = TRUE) #colocando em ordem para avaliar a distribuicao dos valores


#avalie quantas famílias e qual a abundância de cada uma no dataframe

colnames (peixes)
summary(peixes$FAMILY)
levels(peixes$FAMILY)
sort(summary(peixes$FAMILY), decreasing = TRUE)



#Será que todos esses nomes científicos estão atualmente válidos? Para avaliar isso, vamos usar o pacote rotl que
#é uma interfare para a base de informações do "Open Tree of Life" e oferece diversas funções para 
# manipular dados filogenéticos e taxonômicos.
install.packages("C:/Downloads/rotl_3.1.0.zip", repos = NULL, type = "source")

install.packages("rotl")# pra quem ainda não instalou
library(rotl)


taxa_peixes<-tnrs_match_names(peixes$SPECIES, context_name="Vertebrates") #função para checar o nome 
#das spp de acordo com a base do TOF


tnrs_contexts()#para ver todos os contextos taxonômicos que podem ser usados

taxa_peixes#vamos olhar o arquivo


taxa_peixes[which(taxa_peixes$approximate_match==TRUE),]#vamos olhar apenas para as spp que a função encontrou nomes próximos

levels(peixes$SPECIES)# vamos olhar essa info no nosso dataframe

#Assumindo que a plataforma do TOF é confiável pra o grupo taxonômico de interesse, vamos corrigir 
#os nomes científicos no nosso dataframe

peixes$SPECIES<-as.character(peixes$SPECIES) #primeiro precisamos transformar a coluna dos 
#nomes científicos para o tipo "character" e facilitar a manipulação

peixes[peixes$SPECIES=="Astyanax gr. bimaculatus", "SPECIES"]<- "Astyanax bimaculatus"
peixes[peixes$SPECIES=="Bryconops cf. caudomaculatus", "SPECIES"]<- "Bryconops caudomaculatus" #substituindo o nome
peixes[peixes$SPECIES=="Hemigrammus cf. pretoensis", "SPECIES"]<- "Hemigrammus pretoensis"
peixes[peixes$SPECIES=="Microsternarchus cf. bilineatus", "SPECIES"]<- "Microsternarchus bilineatus"

peixes$SPECIES<-as.factor(peixes$SPECIES) #retornando a variável para o tipo factor
levels(peixes$SPECIES) #checando se a alteração foi bem sucedida

#faça o mesmo para a segunda especie indicada como errada

####Exercício 1####
#abra a planilha nomeada peixes

#veja quantas familias foram registradas
## 29 FAMILIAS

#anote a 5 mais frequentes
##FAMILIAS MAIS FREQUENTES: Characidae (202); Cichlidae (200); 
##Lebiasinidae (124); Erythrinidae (97); Gymnotidae (96).

#veja quantas spp foram registradas
## 148 ESPÉCIES

#anote as 5 mais frequentes
##ESPECIES MAIS FREQUENTES: Hyphessobrycon heterorhabdus (60); Helogenes marmoratus (58); 
##Erythrinus?erythrinus (57); Apistogramma gr. regani (53); Aequidens tetramerus (42).

#verifique se os nomes científicos estão de acordo com a plataforma TOF

#renomeia as sinonímias
peixes[peixes$SPECIES=="Astyanax gr. bimaculatus", "SPECIES"]<- "Astyanax bimaculatus"
peixes[peixes$SPECIES=="Bryconops cf. caudomaculatus", "SPECIES"]<- "Bryconops caudomaculatus" #substituindo o nome
peixes[peixes$SPECIES=="Hemigrammus cf. pretoensis", "SPECIES"]<- "Hemigrammus pretoensis"
peixes[peixes$SPECIES=="Microsternarchus cf. bilineatus", "SPECIES"]<- "Microsternarchus bilineatus"

#reavalie quantas sp existem no dataframe
## 148 ESPÉCIES
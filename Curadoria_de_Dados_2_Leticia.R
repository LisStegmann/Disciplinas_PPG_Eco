####MANIPULANDO E EXPLORANDO DADOS NUMÉRICOS

#vamos avaliar a media do comprimento de asa de casa espécie
#pra isso, primeiro precisarmos ter certeza que a variavel esta sendo lida pelo R como numerica

peixes<- read.csv("peixes.csv", sep=';', stringsAsFactors = TRUE)

peixes$NUMBER_OF_RECORDS

# precisando entao transformar a variavel em numerica
peixes$NUMBER_OF_RECORDS<-as.numeric(as.character(peixes$)

#vamos olhar agora a mesma coluna novamente
peixes$NUMBER_OF_RECORDS
summary(peixes$NUMBER_OF_RECORDS)

#vamos explorar as variáveis numéricas para detectar possíveis erros. Vamos começar 
#agregando os valores de comprimento da asa por espécie, usando a fundação "aggregate"

aggregate(x=peixes$NUMBER_OF_RECORDS, by = list(peixes$SPECIES), FUN = sum)

#ops, a função não conseguiu realizar a média de algumas spp. Pq será? Vamos olhar os valores das
#spp que a função está retornando como NA

aves[which(aves$NOME.CIENTIFICO=="ARREMON TACITURNUS"),"COMPRIMENTO.TARSO...MM."]

#Isso acontece porque essas espécies possuem valores inválidos ("NA"). Vamos precisar remover pra seguir
#podemos fazer isso usando a função na.action

linhas_com_na<-na.action(na.exclude(aves$COMPRIMENTO.TARSO...MM.))

#agora criamos um novo objeto removendo essas linhas
peixes_sem_na<-peixes[-linhas_com_na,]

#aplicamos a função de agragação novamente, usando o novo dataframe

media_peixes<-aggregate(x=peixes_sem_na$NUMBER_OF_RECORDS, by = list(peixes_sem_na$SPECIES), FUN = mean)

ab_peixe<-aggregate(x=peixes_sem_na$NUMBER_OF_RECORDS, by = list(peixes_sem_na$SPECIES), FUN = sum)



#vamos colocar em ordem (pacote dplyr)
install.packages("dplyr")
library(dplyr)
media_peixes %>% 
  arrange(x)

ab_peixe %>%
  arrange(x)

#Exercício 2
#Escolha outras duas variaveis numéricas e faça o mesmo exercício nos dados de aves
#anote quais sao as 5 especies com menores medias
#Some a abundancia por especies de peixes
#para somar, use o "sum" no argumento FUN
#anote as 10 mais abundantes e quantas ocorreram apenas uma vez
#as informações de ocorrencia estao na coluna number of records

#### Resposta das atividades ####
####  AVES ###
#Variável: COMPRIMENTO.CAUDA
#Cinco espécies com menores médias: PHAETHORNIS RUPURUMII (4.0), PICUMNUS AURIFRONS (25.0), PIPRA FASCIICAUDA (27.34), MYRMOTHERA BERLEPSCHI (29.57), PLATYRINCHUS CORONATUS (30.97)

#Variável: COMPRIMENTO.TARSO...MM.
#Cinco espécies com menores médias: PHAETHORNIS RUPURUMII (0.50), THRENETES LEUCURUS (4.83), CHIONOMESA FIMBRIATA (5.10), HELIODOXA AURESCENS (5.50), CHLOROCERYLE AENEA (8.33)

### PEIXES ###
#As 10 espécies mais abundantes: Hyphessobrycon heterorhabdus (7072), Copella arnoldi (3087), Apistogramma gr. regani (2541), Elachocharax pulcher (1857), Microcharacidium weitzmani (1049),
#Hemigrammus bellottii (932), Hemigrammus ocellifer (902), Hypopygus lepturus (828), Iguanodectes rachovii (771), Copella callolepis  (658)

#Apenas 15 espécies apareceram apenas uma vez, sendo elas: 
#Belonion sp.
#Brachyhypopomus sp.
#Bryconops sp.
#Carnegiella marthae
#Denticetopsis seducta
#Gymnotus anguillaris
#Hemigrammus cf. belottii
#Hyphessobrycon aff. troemnerus
#Melanocharacidium dispilomma
#Microglanis poecilus
#Phenacogaster gr. pectinatus
#Phenacogaster pectinatus
#Pimelodella cristata 
#Potamorrhaphis guianensis 
#Steatogenys elegans
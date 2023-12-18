####MANIPULANDO E EXPLORANDO DADOS NUMÉRICOS

#vamos avaliar a media do comprimento de asa de casa espécie
#pra isso, primeiro precisarmos ter certeza que a variavel esta sendo lida pelo R como numerica

aves
aves$COMPRIMENTO.ASA..MM.

# precisando entao transformar a variavel em numerica
aves$COMPRIMENTO.ASA..MM. <- as.numeric(as.character(aves$COMPRIMENTO.ASA..MM.))

#vamos olhar agora a mesma coluna novamente
summary(aves$COMPRIMENTO.ASA..MM.)

#vamos explorar as variáveis numéricas para detectar possíveis erros. Vamos começar 
#agregando os valores de comprimento da asa por espécie, usando a fundação "aggregate"

aggregate(x=aves$COMPRIMENTO.ASA..MM., by = list(aves$NOME.CIENTIFICO), FUN = mean)

#ops, a função não conseguiu realizar a média de algumas spp. Pq será? Vamos olhar os valores das
#spp que a função está retornando como NA

aves[which(aves$NOME.CIENTIFICO=="WILLISORNIS VIDUA"), "COMPRIMENTO.ASA..MM."]

#Isso acontece porque essas espécies possuem valores inválidos ("NA"). Vamos precisar remover pra seguir
#podemos fazer isso usando a função na.action

linhas_com_na<-na.action(na.exclude(aves$COMPRIMENTO.ASA..MM.))

#agora criamos um novo objeto removendo essas linhas
aves_sem_na<-aves[-linhas_com_na,]

#aplicamos a função de agragação novamente, usando o novo dataframe

media_aves<-aggregate(x=aves_sem_na$COMPRIMENTO.ASA..MM., by = list(aves_sem_na$NOME.CIENTIFICO), FUN = mean)


#vamos colocar em ordem (pacote dplyr)

media_aves %>% 
  arrange(x)


## comprimento da cauda
aves
aves$COMPRIMENTO.CAUDA..MM.

# precisando entao transformar a variavel em numerica
aves$COMPRIMENTO.CAUDA..MM. <- as.numeric(as.character(aves$COMPRIMENTO.CAUDA..MM.))

#vamos olhar agora a mesma coluna novamente
summary(aves$COMPRIMENTO.CAUDA..MM.)

#vamos explorar as variáveis numéricas para detectar possíveis erros. Vamos começar 
#agregando os valores de comprimento da asa por espécie, usando a fundação "aggregate"

aggregate(x=aves$COMPRIMENTO.CAUDA..MM., by = list(aves$NOME.CIENTIFICO), FUN = mean)

#ops, a função não conseguiu realizar a média de algumas spp. Pq será? Vamos olhar os valores das
#spp que a função está retornando como NA

aves[which(aves$NOME.CIENTIFICO=="AUTOMOLUS PARAENSIS"), "COMPRIMENTO.CAUDA..MM."]

#Isso acontece porque essas espécies possuem valores inválidos ("NA"). Vamos precisar remover pra seguir
#podemos fazer isso usando a função na.action

linhas_com_na<-na.action(na.exclude(aves$COMPRIMENTO.CAUDA..MM.))

#agora criamos um novo objeto removendo essas linhas
aves_sem_na<-aves[-linhas_com_na,]

#aplicamos a função de agragação novamente, usando o novo dataframe

media_aves<-aggregate(x=aves_sem_na$COMPRIMENTO.CAUDA..MM., by = list(aves_sem_na$NOME.CIENTIFICO), FUN = mean)


#vamos colocar em ordem (pacote dplyr)

media_aves %>% 
  arrange(x)

## peso 1
aves
aves$PESO.1..G

# precisando entao transformar a variavel em numerica
aves$PESO.1..G. <- as.numeric(as.character(aves$PESO.1..G))

#vamos olhar agora a mesma coluna novamente
summary(aves$PESO.1..G)

#vamos explorar as variáveis numéricas para detectar possíveis erros. Vamos começar 
#agregando os valores de comprimento da asa por espécie, usando a fundação "aggregate"

aggregate(x=aves$PESO.1..G, by = list(aves$NOME.CIENTIFICO), FUN = mean)

#ops, a função não conseguiu realizar a média de algumas spp. Pq será? Vamos olhar os valores das
#spp que a função está retornando como NA

aves[which(aves$NOME.CIENTIFICO=="PIPRA FASCIICAUDA"), "PESO.1..G"]

#Isso acontece porque essas espécies possuem valores inválidos ("NA"). Vamos precisar remover pra seguir
#podemos fazer isso usando a função na.action

linhas_com_na<-na.action(na.exclude(aves$PESO.1..G))

#agora criamos um novo objeto removendo essas linhas
aves_sem_na<-aves[-linhas_com_na,]

#aplicamos a função de agragação novamente, usando o novo dataframe

media_aves<-aggregate(x=aves_sem_na$PESO.1..G, by = list(aves_sem_na$NOME.CIENTIFICO), FUN = mean)


#vamos colocar em ordem (pacote dplyr)

media_aves %>% 
  arrange(x)



## ABUNDÂNCIA DE PEIXES POR ESPÉCIE
peixes
peixes$NUMBER_OF_RECORDS

# precisando entao transformar a variavel em numerica
peixes$NUMBER_OF_RECORDS <- as.numeric(as.character(peixes$NUMBER_OF_RECORDS))

#vamos olhar agora a mesma coluna novamente
summary(peixes$NUMBER_OF_RECORDS)

#vamos explorar as variáveis numéricas para detectar possíveis erros. Vamos começar 
#agregando os valores de comprimento da asa por espécie, usando a fundação "aggregate"

aggregate(x=peixes$NUMBER_OF_RECORDS, by = list(peixes$SPECIES), FUN=sum)




#aplicamos a função de agragação novamente, usando o novo dataframe

sum_peixes<-aggregate(x=peixes$NUMBER_OF_RECORDS, by = list(peixes$SPECIES), FUN=sum)


#vamos colocar em ordem (pacote dplyr)

sum_peixes %>% 
  arrange(x)




####Exercício 2####
#Escolha outras duas variaveis numéricas e faça o mesmo exercício nos dados de aves

#anote quais sao as 5 especies com menores medias PARA COMPRIMENTO DA CAUDA:
## CACICUS CELA (13.00000); CISSOPIS LEVERIANUS (15.00000); CELEUS ELEGANS (16.00000); 
## XIPHOCOLAPTES PROMEROPIRHYNCHUS (17.00000); DENDROCOLAPTES RETENTUS (27.00000).

#anote quais sao as 5 especies com menores medias PARA PESO 1:
## CACICUS CELA (11.00000); PIAYA CAYANA (31.00000); HIERASPIZA SUPERCILIOSA (34.00000);
## LEPTOTILA VERREAUXI (47.66667); XIPHOCOLAPTES PROMEROPIRHYNCHUS (64.00000)

#Some a abundancia por especies de peixes
#para somar, use o "sum" no argumento FUN


#anote as 10 mais abundantes e quantas ocorreram apenas uma vez
## AS 10 MAIS ABUNDANTES: Hyphessobrycon heterorhabdus (7106); Copella arnoldi (3408); 
## Apistogramma gr. regani (2790);Elachocharax pulcher (1857); Microcharacidium weitzmani (1049); 
## Hemigrammus bellottii (932);Iguanodectes rachovii (923); Hemigrammus ocellifer (902);
## Hypopygus lepturus (828); Pyrrhulina aff. brevis (703).

## 17 ESPÉCIES OCORRERAM APENAS UMA VEZ: Belonion sp.; Brachyhypopomus sp.; Bryconops cf. caudomaculatus; Bryconops inpai;
## Bryconops sp.;Carnegiella marthae; Denticetopsis seducta; Gymnotus anguillaris; Hemigrammus cf. belottii; 
## Hyphessobrycon aff. troemnerus; Jupiaba pirana; Microglanis poecilus; Phenacogaster pectinatus; Pimelodella cristata;
## Potamorrhaphis guianensis; Steatogenys elegans
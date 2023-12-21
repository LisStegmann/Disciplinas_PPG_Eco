
####MANIPULANDO E EXPLORANDO DADOS NUMÉRICOS

#vamos avaliar a media do comprimento de asa de casa espécie
#pra isso, primeiro precisarmos ter certeza que a variavel esta sendo lida pelo R como numerica

aves$COMPRIMENTO.ASA..MM.

# precisando entao transformar a variavel em numerica
aves$COMPRIMENTO.ASA..MM. <- as.numeric(aves$COMPRIMENTO.ASA..MM.)


summary(aves$COMPRIMENTO.ASA..MM.)

summary(aves)


#vamos olhar agora a mesma coluna novamente
aves$COMPRIMENTO.ASA..MM.

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

library(dplyr)

media_aves %>% 
  arrange(x)


#Exercício 2

#Escolha outras duas variaveis numéricas e faça o mesmo exercício nos dados de aves


#anote quais sao as 5 especies com menores medias para COMPRIMENTO DE CAUDA:
# CACICUS CELA (13.00000); CISSOPIS LEVERIANUS (15.00000); CELEUS ELEVGANS (16.00000); 
# XIPHOCOLAPTEUS PROMEROPIRHYNCHUS (17.00000); DENDROCOLAPTES RETENTUS (27.00000). 

#anote quais sao as 5 especies com menores medias para PESO 1:
#CACICUS CELA (11.00000); PIAYA CAYANA (31.00000); HIERASPIZA SUPERCILIOSA (34.00000); 
# LEPTOTILA VERREAUXI (47.00000); XIPHOCOLAPTES PROMEROPIRHYNCHUS (64.00000) 

#Some a abundancia por especies de peixes 

#para somar, use o "sum" no argumento FUN

#anote as 10 mais abundantes e quantas ocorreram apenas uma vez
# As 10 mais abundantes: Hyphessobrycon heterochabdus (7106); copella arnoldi (3408); 
# Apistogramma gr. regani (2790); Elachocharax pulcher (1857); Microcharacidium weitzmani (1049); 
# Hypopygus lepturus (828); Pyrrhulina aff.brevis (703). 

## 17 ESPECIES OCORRERAM APENAS UMA VEZ: Brachyhypopomus sp; Bryconops cf. caudomacula;
## Bryconops.sp; Carnegiella marthae; Denticetopsis seducta; Gymnotus anguillaris; Hemigrammus cf.; 
#Hyphessobrycon aff.troemnerus; Jupiaba pirana; Microglanis poecilus; Phenacogaster pectinatus; 
#Potamorrhaphis guianensis; Steatogenys elegans 

#Escolha outras duas variaveis numéricas e faça o mesmo exercício nos dados de aves

#vamos avaliar a media do comprimento de asa de casa espécie
#pra isso, primeiro precisarmos ter certeza que a variavel esta sendo lida pelo R como numerica


# RESOLUÇÃO DO EXERCÍCIO 

aves$PESO.1..G.


# precisando entao transformar a variavel em numerica
aves$PESO.1..G. <- as.numeric(aves$NOME.CIENTIFICO)


summary(aves$PESO.1..G.)

summary(aves)


#vamos olhar agora a mesma coluna novamente
aves$PESO.1..G.

#vamos explorar as variáveis numéricas para detectar possíveis erros. Vamos começar 
#agregando os valores de comprimento da asa por espécie, usando a fundação "aggregate"

aggregate(x=aves$PESO.1..G., by = list(aves$NOME.CIENTIFICO), FUN = sum)

#ops, a função não conseguiu realizar a média de algumas spp. Pq será? Vamos olhar os valores das
#spp que a função está retornando como NA

aves[which(aves$NOME.CIENTIFICO=="WILLISORNIS VIDUA"), "COMPRIMENTO.ASA..MM."]

#Isso acontece porque essas espécies possuem valores inválidos ("NA"). Vamos precisar remover pra seguir
#podemos fazer isso usando a função na.action

linhas_com_na<-na.action(na.exclude(aves$PESO.1..G.))

#agora criamos um novo objeto removendo essas linhas
aves_sem_na<-aves[-linhas_com_na,]

#aplicamos a função de agragação novamente, usando o novo dataframe

media_aves<-aggregate(x=aves_sem_na$PESO.1..G, by = list(aves_sem_na$NOME.CIENTIFICO), FUN = mean)

media_aves %>%
+    arrange(x)

#vamos colocar em ordem (pacote dplyr)

library(dplyr)

media_aves %>% 
  arrange(x)
 

 #Exercício 3
 #Avalie a distribuição de comprimento da asa das aves por periodo sazonal
#Avalie a abundância de espécies de peixes por estado
#dica: os dados de abundancia estão na coluna number_of_records)
 
#Avalie a distribuição de comprimento da asa das aves por periodo sazonal

aves <- read.csv("AVES.csv", sep = ";", stringsAsFactors = TRUE)
summary(aves$COMPRIMENTO.ASA..MM.)

aves$COMPRIMENTO.ASA..MM. <- as.numeric(aves$COMPRIMENTO.ASA..MM.)
colnames(aves)

aves %>%
  ggplot(aes(x=PERIODO.SAZONAL, y=COMPRIMENTO.ASA..MM., fill= PERIODO.SAZONAL)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.3) +
  geom_jitter(color="black", size=0.9, alpha=0.3) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Comprimento da asa por familia") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  
#Avalie a abundância de espécies de peixes entre eventos climaticos (el nino e el nina)

peixes <- read.csv("peixes.csv", sep=';', stringsAsFactors = TRUE)

colnames(peixes)

peixes1<-aggregate(NUMBER_OF_RECORDS ~ SITE + Evento, data=peixes, sum)

peixes <- peixes %>%
  mutate(Evento = case_when(
    COL_END_YR %in% 2015:2015  ~ "el_nino",
    COL_END_YR %in% 2013:2013 ~ "la_nina"))
colnames(peixes)

peixes1<-aggregate(NUMBER_OF_RECORDS ~ SITE + Evento, data=peixes, sum) 


ggplot(peixes1, aes(x = Evento, y = NUMBER_OF_RECORDS, color=Evento)) +
  geom_point(position = position_jitter(width = 0.2, height = 0), size = 3) +
  labs(title = "Distribuição da Riqueza de Espécies por Local e Categoria",
       x = "evento",
       y = "abundancia") +
  theme_minimal()

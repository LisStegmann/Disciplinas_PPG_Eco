####EXPLORANDO VISIAULMENTE OS DADOS######

library(tidyverse)
install.packages("hrbrthemes")
library(hrbrthemes)
install.packages("viridis")
library(viridis)

#Antes de iniciarmos as análises estatísticas, é muito importante avaliarmos
#a distribuição dos dados visualmente para compreender nossos "outliers"
#e também corrigir possíveis erros de digitação. 

#Vamos criar um bloxplot para observar a distribuição dos valores
#comprimento da asa por família de ave

#primeiro é preciso ter certeza que os valores estão sendo lidos como
#números
aves <- read.csv("AVES.csv", sep=';', stringsAsFactors = TRUE)

summary(aves$COMPRIMENTO.ASA..MM.)
#precisamos então alterar o tipo de dados
aves$COMPRIMENTO.ASA..MM. <- as.numeric(as.character(aves$COMPRIMENTO.ASA..MM.))

#agora podemos gerar o gráfico
aves %>%
  ggplot( aes(x=FAMILIA, y=COMPRIMENTO.ASA..MM., fill=FAMILIA)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.3) + #Muda a transp. do box.
  geom_jitter(color="black", size=0.9, alpha=0.3) + #Muda a transparencia dos pontos
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Comprimento da asa por família") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

#mude os valores de alpha e a cor para ver o que acontece
#vamos interpretar os boxplots

#podemos observar que muitas familias tem valores proximos a zero
#mas faz sentido?
#vamos olhar os valores para uma familia

aves[aves$FAMILIA=="PIPRIDAE", "COMPRIMENTO.ASA..MM."]

#Vamos olhar se esse individuo era um juvenil ou adulto

aves[aves$FAMILIA=="PIPRIDAE" & aves$COMPRIMENTO.ASA..MM.<20, "IDADE" ]

aves[37,]

#Se temos certeza que esses indivíduos foram digitados errados, podemos
#remove-los do dataframe

linhas_remove<-which(aves$FAMILIA=="PIPRIDAE" & aves$COMPRIMENTO.ASA..MM.<12)


#removendo a linha
aves_novo<-aves[-linhas_remove,]
aves_novo[aves_novo$FAMILIA=="PIPRIDAE", "COMPRIMENTO.ASA..MM."]

#podemos fazer isso para todos os valores menor que 10 da coluna de comprimento
#de asa, como seria?

#Vamos plotar novamente, agora usando o novo dataframe
aves_novo %>%
  ggplot( aes(x=FAMILIA, y=COMPRIMENTO.ASA..MM., fill=FAMILIA)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.3) + #Muda a transp. do box.
  geom_jitter(color="black", size=0.9, alpha=0.3) + #Muda a transparencia dos pontos
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Comprimento da asa por família") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


#Se temos certeza que uma informação foi digitada errada e queremos corrigir
#sem precisar voltar para a tabela no excel, podemos fazer isso aqui mesmo.
#Suponhamos que o valor abaixo de 10 que estamos encontrado na família
#PIPRIDAE fosse na verdade 100
#poderiamos corrigir da seguinte forma:

aves[999, "COMPRIMENTO.ASA..MM."]
aves[999, "COMPRIMENTO.ASA..MM."]<- 100
aves[aves$FAMILIA=="PIPRIDAE", "COMPRIMENTO.ASA..MM.",]

aves[995, "COMPRIMENTO.ASA..MM."]
aves[995, "COMPRIMENTO.ASA..MM."]<- 63
aves[995, "COMPRIMENTO.ASA..MM."]


#ainda existem valores muitos baixos, nesse caso precisaríamos
#voltar para a base de dados para ter certeza que as infos foram
#digitadas corretamente

#é possivel visualizar que muitos dos valores mais baixos estao 
#associados a uma época de amostragem. Isso pode indicar ou um erro
#de digitacao/interpretacao dos dados por quem estava digitando
#ou uma agregação de individuos juvenis em um periodo
#vamos avaliar os dados

#vamos agora avaliar o numero de especies de peixes por local
#vamos usar novamente a funcao aggregate para fazer isso

sumario_peixes<-aggregate(OCCUR ~ SITE + STATE, data=peixes, sum)  
summary(sumario_peixes)
#nesse caso, estamos incluindo também a coluna estado



###
getwd()

#função "write
write.csv(sumario_peixes, "D:/OneDrive/Letícia Correia/Doutorado/Disciplinas/Curadoria de dados ecológicos/Disciplinas_PPG_Eco-main/sumario_peixes.csv", row.names = FALSE)

#vamos visualizar os dados

ggplot(sumario_peixes, aes(x = STATE, y = OCCUR, color=STATE)) +
  geom_point(position = position_jitter(width = 0.2, height = 0), size = 3) +
  labs(title = "Distribuição da Riqueza de Espécies por Local e Categoria",
       x = "Local",
       y = "Riqueza") +
  theme_minimal()


#Exercício 3

#Avalie a distribuição de comprimento da asa das aves por periodo sazonal

#Avalie a abundância de espécies entre eventos climáticas (el nino, el nina)

#Avalie de os valores abaixo de 20 da família FURNARIIDAE (aves) são de 
#indíduos juvenis ou adultos

#Remova todos os valores abaixo de 10 da coluna comprimento de asa


## Resolução da atividade ##
# 1 Avalie a distribuição de comprimento da asa das aves por periodo sazonal

aves_novo %>%
  ggplot( aes(x=PERIODO.SAZONAL, y=COMPRIMENTO.ASA..MM., fill=FAMILIA)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.3) + #Muda a transp. do box.
  geom_jitter(color="black", size=0.9, alpha=0.3) + #Muda a transparencia dos pontos
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Comprimento da asa por período sazonal") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# 2 Avalie a abundância de espécies entre eventos climáticas (el nino, el nina)

peixes <- read.csv("peixes.csv", sep=';', stringsAsFactors = TRUE)

peixes1<-aggregate(NUMBER_OF_RECORDS ~ SITE + EVENTO, data=peixes, sum)

ggplot(peixes1, aes(x = EVENTO, y = NUMBER_OF_RECORDS, color=EVENTO)) +
  geom_point(position = position_jitter(width = 0.2, height = 0), size = 3) +
  labs(title = "Abundancia de espécie entre eventos climáticos",
       x = "Local",
       y = "Riqueza") +
  theme_minimal()

# 3 Avalie se os valores abaixo de 20 da família FURNARIIDAE (aves) são de 
#indíduos juvenis ou adultos

which(aves$FAMILIA=="BUCCONIDAE" & as.numeric(aves$COMPRIMENTO.ASA..MM.)<=20)
aves$IDADE[661]
aves<-aves[-661,]

# 4 Remova todos os valores abaixo de 10 da coluna comprimento de asa
aves <- read.csv("AVES.csv", sep=';', stringsAsFactors = TRUE)

linhas_remove1<-which(aves$FAMILIA=="TURDIDAE" & as.numeric(aves$COMPRIMENTO.ASA..MM.)<10)
aves_novo2<-aves[-linhas_remove1,]
write.csv(aves_novo2,"aves_novo.csv")


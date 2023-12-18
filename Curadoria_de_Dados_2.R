####MANIPULANDO E EXPLORANDO DADOS NUMÉRICOS

#vamos avaliar a media do comprimento de asa de casa espécie
#pra isso, primeiro precisarmos ter certeza que a variavel esta sendo lida pelo R como numerica


aves$COMPRIMENTO.ASA..MM.

# precisando entao transformar a variavel em numerica
aves$COMPRIMENTO.ASA..MM. <- as.numeric(as.character(aves$COMPRIMENTO.ASA..MM.))

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

abund_aves %>% 
  arrange(x)


#Exercício 2
#Escolha outras duas variaveis numéricas e faça o mesmo exercício nos dados de aves
#anote quais sao as 5 especies com menores medias
#Some a abundancia por especies de peixes
#para somar, use o "sum" no argumento FUN
#anote as 10 mais abundantes e quantas ocorreram apenas uma vez


####EXPLORANDO VISIAULMENTE OS DADOS######

library(tidyverse)
install.packages("hrbrthemes")
library(hrbrthemes)
install.packages("viridis")
library(viridis)

#vamos criar um bloxplot para observar a distribuição dos valores
#comprimento da asa por família de ave
aves_sem_na %>%
ggplot( aes(x=FAMILIA, y=COMPRIMENTO.ASA..MM., fill=FAMILIA)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="black", size=0.9, alpha=0.9) +
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

aves_sem_na[aves_sem_na$FAMILIA=="CARDINALIDAE", "COMPRIMENTO.ASA..MM."]

#precisamos remover os zeros da planilha, ja que nao faz
#sentido um individuo ter comprimento de asa = a zero

aves_novo<-aves_sem_na[-which(aves_sem_na$COMPRIMENTO.ASA..MM.< 5),]

#avaliando se os zeros foram mesmo apagados
which(aves_novo$COMPRIMENTO.ASA..MM.< 5)

#plotando novamente, agora usando o novo dataframe

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
#nesse caso, estamos incluindo também a coluna estado

#vamos visualizar os dados

 ggplot(sumario_peixes, aes(x = STATE, y = OCCUR, color=STATE)) +
  geom_point(position = position_jitter(width = 0.2, height = 0), size = 3) +
  labs(title = "Distribuição da Riqueza de Espécies por Local e Categoria",
       x = "Local",
       y = "Riqueza") +
   theme_minimal()
 

 #Exercício 3
 #Avalie a distribuição de comprimento da asa das aves por periodo sazonal
#Avalie a abundância de espécies de peixes por estado
#dica: os dados de abundancia estão na coluna number_of_records)
 

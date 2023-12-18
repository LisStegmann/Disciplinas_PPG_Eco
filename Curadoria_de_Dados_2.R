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
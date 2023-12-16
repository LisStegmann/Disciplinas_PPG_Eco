#Verificar pasta do diretório
install.packages("vegan")
library(vegan)
library(ggplot2)

aves <- read.csv("AVES.csv", sep=';', stringsAsFactors = TRUE) #Abrirndo e criando o dataframe

summary(aves) #Avaliando a natureza da informação contida no objeto criado. Testem abrir o arquivo 
#alternando o #último arguento da função entre TRUE e FALSE e vejam a diferença. 

      ####MANIPULANDO E EXPLORANDO DADOS TEXTUAIS E CATEGÓRICOS####


 #Vamos avaliar quantas espécies existem na nossa planilha.
levels(aves$NOME.CIENTIFICO)#Para isso, vamos usar a função levels. Ela nos indica a variabilidade de uma informação 
                            #categórica, ou seja, os diferentes níveis da informação. 


summary(aves$NOME.CIENTIFICO)) #essa função nos mostra quantas vezes cada categoria aparece no dataframe, ou 
                              #seja, a frequência de cada spp

#Será que todos esses nomes científicos estão atualmente válidos? Para avaliar isso, vamos usar o pacote rotl que
#é uma interfare para a base de informações do "Open Tree of Life" e oferece diversas funções para 
# manipular dados filogenéticos e taxonômicos.

install.packages("rotl")# pra quem ainda não instalou
library(rotl)


taxa_aves<-tnrs_match_names(aves$NOME.CIENTIFICO, context_name="Birds") #função para checar o nome 
#das spp de acordo com a base do TOF

taxa_aves#vamos olhar o arquivo

tnrs_contexts()#para ver todos os contextos taxonômicos que podem ser usados

taxa_aves[which(taxa_aves$approximate_match==TRUE),]#vamos olhar apenas para as spp que a função encontrou nomes próximos


#Assumindo que a plataforma do TOF é confiável pra o grupo taxonômico de interesse, vamos corrigir 
#os nomes científicos no nosso dataframe


aves$NOME.CIENTIFICO<-as.character(aves$NOME.CIENTIFICO) #primeiro precisamos transformar a coluna dos 
#nomes científicos para o tipo "character" e facilitar a manipulação

aves[aves$NOME.CIENTIFICO=="HABIA RUBRA", "NOME.CIENTIFICO"]<- "HABIAS RUBIA" #substituindo o nome

aves$NOME.CIENTIFICO<-as.factor(aves$NOME.CIENTIFICO) #retornando a variável para o tipo factor
levels(aves$NOME.CIENTIFICO) #checando se a alteração foi bem sucedida

  ####Exercício 1
    #abra a planilha nomeada peixes
    #veja quantas spp foram registradas
    #anote as 5 mais abundantes
    #verifique se os nomes científicos estão de acordo com a plataforma TOF
    #renomeia as sinonímias
    


###SEPARANDO E AGREGANDO INFORMAÇÕES

#Vamos precisar usar o pacote tydyverse
install.packages("tidyverse") #pra quem ainda não instalou  
library(tidyverse)

#quebrar a informação de data

dados_2<- dados %>% separate(DATA, 
                             into = c("DIA", "MES", "ANO"), 
                             sep = "/") 

factor(dados_2$ANO)


####MANIPULANDO E EXPLORANDO DADOS NUMÉRICOS
#Podemos obversar que colunas com variáveis numéricas estão classificadas como 
# do tipo "character", ou seja, dados textuais. Porque será? 
#vamos pegar uma coluna numérica como teste e observar os valores.

dados$COMPRIMENTO.ASA..MM.

#observamos que os valores estão entre "" o que demonstra que estão sendo 
#lidos como texto. Também temos informações textuais na mesma coluna ("NC")
#Precisamos remover essa informação para que o R leia a coluna como numérica.

#transformar em numerico
dados$COMPRIMENTO.ASA..MM. <- as.numeric(as.character(dados$COMPRIMENTO.ASA..MM.))

#vamos olhar agora a mesma coluna novamente
dados$COMPRIMENTO.ASA..MM.

summary(dados)

#O que mudou?

#Faça o mesmo exercício para comprimento do tarso e da cauda


#vamos explorar as variáveis numéricas para detectar possíveis erros. Vamos começar 
#agregando os valores de comprimento da asa por espécie, usando a fundação "aggregate"

aggregate(x=dados$COMPRIMENTO.ASA..MM., by = list(dados$NOME.CIENTIFICO), FUN = mean)

#ops, a função não conseguiu realizar a média de algumas spp. Pq será? Vamos olhar os valores das
#spp que a função está retornando como NA

dados[which(dados$NOME.CIENTIFICO=="COPIAR NOME DA SPP AQUI"), "COMPRIMENTO.ASA..MM."]

#Isso acontece porque essas espécies possuem valores inválidos ("NA"). Vamos precisar remover pra seguir
#podemos fazer isso usando a função na.action

linhas_com_na<-na.action(na.exclude(dados$COMPRIMENTO.ASA..MM.))

#agora criamos um novo objeto removendo essas linhas
dados_semna<-dados[-linhas_com_na,]

#aplicamos a função de agragação novamente, usando o novo dataframe

aggregate(x=dados_semna$COMPRIMENTO.ASA..MM., by = list(dados_semna$NOME.CIENTIFICO), FUN = mean)

#Refaça esse exercício para as duas outras colunas que você transformou em numérica

#vamos agora olhar a distribuição desses dados

##distribuição dos dados

colnames(lista_aves)="abundancia"

rownames(lista_aves)
lista_aves_red=cbind(rownames(lista_aves[1:10,]), lista_aves[1:10,]
                     
                     ggplot(lista_aves_red, aes(y =abundancia , x = rownames(lista_aves))) 

#testando alterações
                     
                     




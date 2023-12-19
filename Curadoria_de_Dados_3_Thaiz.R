###SEPARANDO E AGREGANDO INFORMAÇÕES

#Vamos precisar usar o pacote tydyverse
install.packages("tidyverse") #pra quem ainda não instalou  
library(tidyverse)
library(dplyr)

#Vamos usar a função "separate" que nos ajuda a quebrar a informação
#de uma coluna em diversas outras. É muito útil quando temos uma
#coluna onde diversas informações estão agregadas e nos impedindo
#de aplicar alguns filtros  

#vamos olhar a informação da data de coleta das aves
aves <- read.csv("AVES.csv", sep=';', stringsAsFactors = TRUE)
aves$DATA
#suponhamos que nosso interesse fosse apenas selecionar os dados coletados
#em determinado ano ou determinado mês. Não teríamos como conseguir
#aplicar esse filtro com os dados da forma como estão, agregados.
#vamos então separar os dias, anos e mês em três colunas diferentes
#vamos antes avaliar o número de colunas do nosso dataframe
#quem lembra o codigo?

#agora separando a coluna DATA, nas categorias que constam entre parêntese,
#e indicamos a "/" como o limite onde o dado deve ser quebrado

aves<- aves %>% separate(DATA, 
                           into = c("DIA", "MES", "ANO"), 
                           sep = "/") 

#vamos avaliar o número de colunas após a quebra
colnames(aves)
ncol(aves)
aves$DIA
aves$ANO
#Também podemos usar esse pacote para separar o nome das spp em duas 
#colunas, uma contendo o gênero e o epiteto
#isso é muito útil quando não queremos digitar toda essa informação na planilha
##mãe. Podemos então gerar essas informações mais detalhadas depois, 
#já dentro do R.


aves<- aves %>% separate(NOME.CIENTIFICO, 
                           c("genero", "epiteto"),  
                           fill = "left") 

#vamos ver novamente o numero total de colunas do nosso objeto e o nome
#das colunas
ncol(aves)
colnames(aves)
#vamos chamar apenas a coluna de genus para avaliar a informação criada
aves$genero
summary(aves$genero)
aves$genero<-as.factor(aves$genero)

#agora no lugar de separar, nós vamos unir as informações. Para isso vamos
#usar a função "mutate" que altera nosso dataframe, junto com a função
#"paste" que irá unir as colunas. Digamos, por exemplo, que queremos 
#avaliar o tamanho da asa apenas dos individuos machos e juvenis. 
#Uma forma simples de fazer isso é unir a coluna "idade" e "sexo", para 
#depois elecionar apenas os que temos interesse.

aves<- aves %>%
  mutate(idade_sexo = paste(IDADE, SEXO, sep = " "))

colnames(aves) #vamos olhar onde ele criou a coluna
#vamos acessar apenas a nova coluna para ver a informação criada
aves$idade_sexo
ncol(aves)
#Imaginem agora que desejamos avaliar a variabilidade de alguma informação
#ecológica entre estações do ano mas esquemos de inserir essa informação
#dentro da nossa planilha mãe. A função "case_when" pode ajudar a criar
#uma nova coluna de acordo com informações já presentes no dataframe.

#vamos usar as informações de MÊS já contida na planilha para gerar outra 
#coluna
aves$MES <- as.numeric(aves$MES) #Nesse caso, primeiro precisamos 
#transformar a informação em numérica para que o R possa compreender
#a classificação(como não tem ponto decimal, não precisamos nos preocupar
#em manter os caracteres)

#agora aplicamos a função case_when

aves <- aves %>%
  mutate(Estacao = case_when(
    MES %in% 4:9  ~ "chuvoso",
    MES %in% 1:3 ~ "verao",
    MES %in% 10:12 ~ "chuvoso",
      ))

#vamos olhar agora a nova coluna criada
aves$Estacao




##### exercicio 3 ######
## 1 ##
peixes <- read.csv("peixes.csv", sep=';', stringsAsFactors = TRUE)

colnames(peixes)

peixes<- peixes %>% separate(SPECIES, 
                         c("genero","aff sp", "epiteto"),  
                         fill = "right") 

ncol(peixes)
colnames(peixes)
peixes$genero
summary(peixes$genero)
peixes$genero<-as.factor(peixes$genero)

## 2 ##
aves <- read.csv("AVES.csv", sep=';', stringsAsFactors = TRUE)
colnames(aves)
aves$HORARIO

aves<- aves %>% separate(HORARIO, 
                         into = c("HORA", "MINUTOS"), 
                         sep = ":") 

colnames(aves)
ncol(aves)
aves$HORA
aves$MINUTOS


## 3 ##
aves <- read.csv("AVES.csv", sep=';', stringsAsFactors = TRUE)
colnames(aves)
aves<- aves %>% separate(DATA, 
                         into = c("DIA", "MES", "ANO"), 
                         sep = "/") 

colnames(aves)
ncol(aves)
aves$DIA
aves$ANO

aves <- aves %>%
  mutate(Estacao = case_when(
    ANO %in% 2012:2016  ~ "chuvoso",
    ANO %in% 2017:2018 ~ "verao",
    ANO %in% 2019:2021 ~ "chuvoso",
  ))

aves$Estacao


## 4 ##
peixes <- read.csv("peixes.csv", sep=';', stringsAsFactors = TRUE)
colnames(peixes)
peixes$COL_END_YR
peixes <- peixes %>%
  mutate(Evento = case_when(
    COL_END_YR %in% 2015:2015  ~ "el_nino",
    COL_END_YR %in% 2013:2013 ~ "la_nina"))
colnames(peixes)
peixes$Evento


########EXERCICIO 3#


#1. Usando os dados dos peixes, separe o nome das spp em três colunas 
  #(como várias espécies estão como aff  ou sp, precisamos inserir mais 
  #de duas colunas)
  #use o argumento fill="rigth" para indicar a ordem de preenchimento das colunas
#2. Usando os dados de aves, separe hora e minutos em duas colunas diferentes 
#3. Também usando os dados de aves, crie uma nova coluna unindo as informações do 
  #período sazonal (estacao) e de ano
#4. Usando os dados de peixes, crie uma nova coluna para determinar quais 
  #coletar aconteceram no el nino e quais aconteceram durante o la nina.
  #considere 2015 como el nino e 2013 como la nina

sample()


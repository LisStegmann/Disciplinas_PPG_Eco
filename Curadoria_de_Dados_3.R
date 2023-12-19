###SEPARANDO E AGREGANDO INFORMAÇÕES

#Vamos precisar usar o pacote tydyverse
install.packages("tidyverse") #pra quem ainda não instalou  
library(tidyverse)

#Vamos usar a função "separate" que nos ajuda a quebrar a informação
#de uma coluna em diversas outras. É muito útil quando temos uma
#coluna onde diversas informações estão agregadas e nos impedindo
#de aplicar alguns filtros  

#vamos olhar a informação da data de coleta das aves

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


#vamos chamar apenas a coluna de genus para avaliar a informação criada


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
    MES %in% 1:3 ~ "verão",
    MES %in% 10:12 ~ "chuvoso",
      ))

#vamos olhar agora a nova coluna criada
aves$Estacao


########EXERCICIO 3#


#1. Usando os dados dos peixes, separe o nome das spp em três colunas 
  #(como várias espécies estão como aff  ou sp, precisamos inserir mais 
  #de duas colunas)
  #use o argumento fill="rigth" para indicar a ordem de preenchimento das colunas
#2. Usando os dados de aves, separe hora e minutos em duas colunas diferentes 
#3. Também usando os dados de aves, crie uma nova coluna unindo as informações do 
  #período sazonal e de ano
#4. Usando os dados de peixes, crie uma nova coluna para determinar quais 
  #coletar aconteceram no el nino e quais aconteceram durante o la nina.
  #considere 2015 como el nino e 2013 como la nina

sample()


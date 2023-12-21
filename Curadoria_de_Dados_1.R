
#Verificar pasta do diretório
install.packages("vegan")
install.packages("ggplot2")
library(vegan)
library(ggplot2)

aves <- read.csv("aves.csv", sep=';', stringsAsFactors = TRUE) #Abrirndo e criando o dataframe
nrow(aves) #para saber número de linhas
ncol(aves) #para saber número de colunas
colnames(aves) #para saber nome das colunas

#vemos que o R está considerando algumas colunas vazias como parte do dataframe. Podemos apagá-las 
#para manejar melhor os dados
aves<- aves[,1:13]

colnames(aves) #vamos olhar novamente o nome das colunas

summary(aves) #Avaliando a natureza da informação contida no objeto criado. Testem abrir o arquivo 

#alternando o último arguento da função entre TRUE e FALSE e vejam a diferença. 

####MANIPULANDO E EXPLORANDO DADOS TEXTUAIS E CATEGÓRICOS####


#Vamos avaliar quantas espécies existem na nossa planilha.
levels(aves$SPECIES)#Para isso, vamos usar a função levels. Ela nos indica a variabilidade de uma informação 
#categórica, ou seja, os diferentes níveis da informação. 


summary(aves$SPECIES) #essa função nos mostra quantas vezes cada categoria aparece no dataframe, ou 
#seja, a frequência de cada spp

sort(summary(aves$SPECIES), decreasing = TRUE) #colocando em ordem para avaliar a distribuicao dos valores


#avalie quantas famílias e qual a abundância de cada uma no dataframe


#Será que todos esses nomes científicos estão atualmente válidos? Para avaliar isso, vamos usar o pacote rotl que
#é uma interfare para a base de informações do "Open Tree of Life" e oferece diversas funções para 
# manipular dados filogenéticos e taxonômicos.

install.packages("rotl")# pra quem ainda não instalou
library(rotl)


taxa_aves<-tnrs_match_names(aves$SPECIES, context_name="Birds") #função para checar o nome 
#das spp de acordo com a base do TOF

tnrs_contexts()#para ver todos os contextos taxonômicos que podem ser usados

taxa_aves#vamos olhar o arquivo


taxa_aves[which(taxa_aves$approximate_match==TRUE),]#vamos olhar apenas para as spp que a função encontrou nomes próximos

levels(aves$SPECIES)# vamos olhar essa info no nosso dataframe

#Assumindo que a plataforma do TOF é confiável pra o grupo taxonômico de interesse, vamos corrigir 
#os nomes científicos no nosso dataframe


aves$SPECIES<-as.character(aves$SPECIES) 
#primeiro precisamos transformar a coluna dos 
#nomes científicos para o tipo "character" e facilitar a manipulação

aves[aves$SPECIES=="Aequidens tetramerus", "Anablepsoides micropus"]<- "" #substituindo o nome

aves$NOME.SPECIES<-as.factor(aves$SPECIES) #retornando a variável para o tipo factor
levels(aves$SPECIES) #checando se a alteração foi bem sucedida

#faça o mesmo para a segunda especie indicada como errada

####Exercício 1
#abra a planilha nomeada peixes
#veja quantas familias foram registradas
#anote a 5 mais frequentes
#veja quantas spp foram registradas
#anote as 5 mais frequentes
#verifique se os nomes científicos estão de acordo com a plataforma TOF
#renomeia as sinonímias
#reavalie quantas sp existem no dataframe

#RESPOSTAS DO EXERCÍCIO 1

####Exercício 1
#abra a planilha nomeada peixes
#veja quantas familias foram registradas: 7 famílias ( Characidae, Cichlidae, Lebiasinidae, Erythrinidae, Gymnotidae, Crenuchidae e Other). 

#anote a 5 mais frequentes: Characidae, Cichlidae, Lebiasinidae, Erythrinidae, Gymnotidae.


#veja quantas spp foram registradas: 294 espécies 
#anote as 5 mais frequentes: Hyphessobrycon heterorhabdus, Helogenes marmoratus, Erythrinus?erythrinus, Apistogramma gr.regani, Aequidens tetramerus. 


#verifique se os nomes científicos estão de acordo com a plataforma TOF
#[1] "Acanthodoras cataphractus"        "Aequidens epae"                  
#[3] "Aequidens pallidus"               "Aequidens sp. (juvenil)"         
#[5] "Aequidens tetramerus"             "Anablepsoides micropus"          
#[7] "Anablepsoides ornatus"            "Anablepsoides urophthalmus"      
#[9] "Ancistrus verecundus"             "Apistogramma gr. agassizii"      
#[11] "Apistogramma gr. regani"          "Apistogramma?agassizii"          
#[13] "Astyanax gr. bimaculatus"         "Bario steindachneri"             
#[15] "Batrochoglanis raninus"           "Belonion sp."                    
#[17] "Brachyhypopomus beebei"           "Brachyhypopomus brevirostris"    
#[19] "Brachyhypopomus sp."              "Brachyhypopomus sp. (juvenil)"   
#[21] "Brachyhypopomus sp.2"             "Brachyhypopomus sp.3"            
#[23] "Brachyhypopomus sullivani"        "Bryconops cf. caudomaculatus"    
#[25] "Bryconops inpai"                  "Bryconops munduruku"             
#[27] "Bryconops sp."                    "Bryconops sp1"                   
#[29] "Bunocephalus sp1"                 "Callichthys callichthys"         
#[31] "Carnegiella marthae"              "Carnegiella strigata"            
#[33] "Characidium cf. etheostoma"       "Characidium sp. n."              
#[35] "Characidium zebra"                "Copella arnoldi"                 
#[37] "Copella callolepis"               "Copella nattereri"               
#[39] "Copella nigrofasciata"            "Corydoras melanistius"           
#[41] "Crenicichla gr. saxatilis"        "Crenicichla inpa"                
#[43] "Crenicichla labrina"              "Crenicichla regani"              
#[45] "Crenicichla sp. (juvenil)"        "Crenuchus?spilurus?"             
#[47] "Curimatopsis crypticus"           "Curimatopsis evelynae"           
#[49] "Denticetopsis epa"                "Denticetopsis seducta"           
#[51] "Eigenmannia aff. trilineata"      "Eigenmannia gr. trilineata"      
#[53] "Eigenmannia sp."                  "Elachocharax pulcher"            
#[55] "Erythrinus?erythrinus"            "Farlowella amazonum"             
#[57] "Fluviphylax sp1"                  "Gladioglanis conquistador"       
#[59] "Gnathocharax steindachneri"       "Gymnorhamphichthys petiti"       
#[61] "Gymnorhamphichthys?rondoni"       "Gymnotus aff. jonasi"            
#[63] "Gymnotus anguillaris"             "Gymnotus carapo"                 
#[65] "Gymnotus cf. anguillaris"         "Gymnotus coatesi"                
#[67] "Gymnotus coropinae"               "Gymnotus gr. carapo"             
#[69] "Gymnotus gr. coropinae"           "Gymnotus gr. pantherinus"        
#[71] "Gymnotus pedanopterus"            "Gymnotus sp. (juvenil)"          
#[73] "Helogenes marmoratus"             "Hemigrammus bellottii "          
#[75] "Hemigrammus cf. belottii"         "Hemigrammus cf. pretoensis"      
#[77] "Hemigrammus levis"                "Hemigrammus ocellifer"           
#[79] "Hemigrammus rodwayi"              "Hemigrammus schmardae"           
#[81] "Hemigrammus sp.1"                 "Hemigrammus sp.2"                
#[83] "Hoplerythrinus unitaeniatus"      "Hoplias malabaricus"             
#[85] "Hoplias sp. (juvenil)"            "Hyphessobrycon aff. melazonatus" 
#[87] "Hyphessobrycon aff. troemnerus"   "Hyphessobrycon copelandi"        
#[89] "Hyphessobrycon heterorhabdus"     "Hypopygus benoneae"              
#[91] "Hypopygus lepturus"               "Hypopygus sp. (juvenil)"         
#[93] "Iguanodectes rachovii"            "Iguanodectes variatus"           
#[95] "Ituglanis amazonicus"             "Jupiaba pirana"                  
#[97] "Knodus cf. victoriae"             "Knodus sp.1"                     
#[99] "Laimosemion cf. dibaphus"         "Laimosemion strigatus"           
#[101] "Lebiasina sp1"                    "Lebiasina sp2"                   
#[103] "Leptophilypnion pusillus"         "Mastiglanis asopos"              
#[105] "Megalechis picta"                 "Megalechis thoracata"            
#[107] "Melanocharacidium dispilomma"     "Melanorivulus cf. modestus"      
#[109] "Microcharacidium eleotrioides"    "Microcharacidium weitzmani"      
#[111] "Microglanis poecilus"             "Microphilypnus ternetzi"         
#[113] "Microsternarchus aff. bilineatus" "Microsternarchus cf. bilineatus" 
#[115] "Moenkhausia collettii"            "Moenkhausia comma"               
#[117] "Moenkhausia oligolepis"           "Monocirrhus polyacanthus"        
#[119] "Nannacara taenia"                 "Nannostomus digrammus"           
#[121] "Nannostomus eques"                "Nannostomus marginatus"          
#[123] "Nannostomus trifasciatus"         "Paracanthopoma sp."              
#[125] "Phenacogaster gr. pectinatus"     "Phenacogaster pectinatus"        
#[127] "Physopyxis ananas"                "Pimelodella cristata"            
#[129] "Pimelodella sp."                  "Poecilocharax weitzmani"         
#[131] "Potamorrhaphis guianensis"        "Priocharax pygmaeus"             
#[133] "Priocharax sp1"                   "Pristella maxillaris"            
#[135] "Pygidianops amphioxus"            "Pyrrhulina aff. brevis"          
#[137] "Pyrrhulina brevis"                "Pyrrhulina sp1"                  
#[139] "Rhamdia?muelleri"                 "Rineloricaria lanceolata"        
#[141] "Rineloricaria sp."                "Scorpiodoras calderonensis"      
#[143] "Steatogenys elegans"              "Sternopygus macrurus"            
#[145] "Synbranchus marmoratus"           "Tetranematichthys wallacei"      
#[147] "Trichomycterus hasemani  "        "Tridentopsis sp1"      

#renomeia as sinonímias:

Hyphessobrycon heterorhabdus    Helogenes marmoratus            Erythrinus?erythrinus 
60                               58                               57 
Apistogramma gr. regani             Aequidens tetramerus       Anablepsoides urophthalmus 
53                               42                               36 
Copella arnoldi           Hemigrammus bellottii             Iguanodectes rachovii 
35                               35                               34 
Hoplias malabaricus           Pyrrhulina aff. brevis     Brachyhypopomus brevirostris 
33                               30                               28 
Hemigrammus ocellifer              Crenuchus?spilurus?           Gymnotus gr. coropinae 
28                               27                               26 
Hypopygus lepturus               Gymnotus coropinae         Gymnotus gr. pantherinus 
21                               19                               19 
Nannacara taenia        Gymnorhamphichthys petiti       Microcharacidium weitzmani 
19                               18                               18 
Carnegiella strigata        Crenicichla gr. saxatilis           Apistogramma?agassizii 
17                               17                               16 
Gymnorhamphichthys?rondoni             Brachyhypopomus sp.2               Aequidens pallidus 
16                               15                               14 
Ituglanis amazonicus    Microcharacidium eleotrioides         Nannostomus trifasciatus 
14                               14                               14 
Apistogramma gr. agassizii Microsternarchus aff. bilineatus           Moenkhausia oligolepis 
13                               13                               13 
Synbranchus marmoratus          Aequidens sp. (juvenil)       Characidium cf. etheostoma 
13                               12                               12 
Laimosemion strigatus        Trichomycterus hasemani                    Gymnotus carapo 
12                               12                               11 
Hemigrammus schmardae           Brachyhypopomus beebei       Eigenmannia gr. trilineata 
11                               10                               10 
Hyphessobrycon aff. melazonatus                Physopyxis ananas                Pyrrhulina brevis 
10                               10                               10 
Callichthys callichthys              Farlowella amazonum       Gnathocharax steindachneri 
9                                9                                9 
Copella nigrofasciata                Denticetopsis epa            Anablepsoides ornatus 
8                                8                                7 
Curimatopsis evelynae       Hemigrammus cf. pretoensis               Paracanthopoma sp. 
7                                6                                6 
Crenicichla sp. (juvenil)           Curimatopsis crypticus         Gymnotus cf. anguillaris 
5                                5                                5 
Hoplias sp. (juvenil)          Hypopygus sp. (juvenil)                Nannostomus eques 
5                                5                                5 
Characidium sp. n.               Copella callolepis                Copella nattereri 
4                                4                                4 
Elachocharax pulcher        Gladioglanis conquistador              Gymnotus gr. carapo 
4                                4                                4 
Gymnotus sp. (juvenil)                      Knodus sp.1         Monocirrhus polyacanthus 
4                                4                                4 
Nannostomus digrammus                   Aequidens epae           Anablepsoides micropus 
4                                3                                3 
Ancistrus verecundus         Astyanax gr. bimaculatus             Brachyhypopomus sp.3 
3                                3                                3 
Gymnotus aff. jonasi            Gymnotus pedanopterus                Hemigrammus levis 
3                                3                                3 
Hemigrammus rodwayi                 Hemigrammus sp.1         Hyphessobrycon copelandi 
3                                3                                3 
Laimosemion cf. dibaphus                    Lebiasina sp1         Leptophilypnion pusillus 
3                                3                                3 
Melanorivulus cf. modestus  Microsternarchus cf. bilineatus           Nannostomus marginatus 
3                                3                                3 
Pimelodella sp.          Poecilocharax weitzmani             Pristella maxillaris 
3                                3                                3 
Rhamdia?muelleri             Sternopygus macrurus        Acanthodoras cataphractus 
3                                3                                2 
Bario steindachneri    Brachyhypopomus sp. (juvenil)                 Bunocephalus sp1 
2                                2                                2 
Characidium zebra                 Crenicichla inpa              Crenicichla labrina 
2                                2                                2 
(Other) 

#reavalie quantas sp existem no dataframe: 100 espécies 


#FAZENDO ANÁLISE PARA FAZER O EXERCÍCIO

#Verificar pasta do diretório
install.packages("vegan")
install.packages("ggplot2")
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


#Será que todos esses nomes científicos estão atualmente válidos? Para avaliar isso, vamos usar o pacote rotl que
#é uma interfare para a base de informações do "Open Tree of Life" e oferece diversas funções para 
# manipular dados filogenéticos e taxonômicos.

install.packages("rotl")# pra quem ainda não instalou
library(rotl)


taxa_peixes<-tnrs_match_names(peixes$SPECIES, context_name="Birds") #função para checar o nome 
#das spp de acordo com a base do TOF

tnrs_contexts()#para ver todos os contextos taxonômicos que podem ser usados

taxa_peixes#vamos olhar o arquivo


taxa_peixes[which(taxa_peixes$approximate_match==TRUE),]#vamos olhar apenas para as spp que a função encontrou nomes próximos

levels(peixes$SPECIES)# vamos olhar essa info no nosso dataframe

#Assumindo que a plataforma do TOF é confiável pra o grupo taxonômico de interesse, vamos corrigir 
#os nomes científicos no nosso dataframe


peixes$SPECIES<-as.character(peixes$SPECIES) 
#primeiro precisamos transformar a coluna dos 
#nomes científicos para o tipo "character" e facilitar a manipulação

peixes[peixes$SPECIES=="Aequidens pallidus", "Aequidens sp. (juvenil)"]<- "" #substituindo o nome

peixes$NOME.SPECIES<-as.factor(peixes$SPECIES) #retornando a variável para o tipo factor
levels(peixes$SPECIES) #checando se a alteração foi bem sucedida

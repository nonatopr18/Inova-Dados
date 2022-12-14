#############################################################################
#                   Sistema de Recomenda??o Caso Geral                      #
#############################################################################
############################################################################
#                   Instalar Bibliotecas                                   #
############################################################################
install.packages("installr")
install.packages("data.table")
install.packages("ggplot2")
install.packages("recommenderlab")
install.packages("countrycode")
############################################################################
#                   Carregar Bibliotecas                                   #
############################################################################
rm(list=ls(all=TRUE))
gc(reset=T)
library (installr)
updateR()
library("data.table")
library("ggplot2")
library("recommenderlab")
library("countrycode")
###########################################################################
#                      Carregar Dados                                     #
###########################################################################
table_in <- read.csv2("C:\\Users\\nonat\\Desktop\\dadosteste12327082022.csv",header=FALSE)
head(table_in)
###########################################################################
#          Descartando as colunas desnecess?rias                          #
###########################################################################
table_users <- table_in[ ,1:2]
##########################################################################
#       Convertendo em tabela de dados                                  #
########################################################################
table_users <- data.table(table_users)
head(table_users)
################################################################################
#        Gerando os usu?rios e os itens                               #
######################################################################
setnames(table_users, 1:2, c("category", "value"))
table_users <- table_users[category %in% c("C","V")]
head(table_users)
################################################################################
#        Definindo a matriz de Ratings                                         #
################################################################################
table_users[,chunk_user:=cumsum(category=="C")]
head(table_users)
################################################################################
#        Gerando uma tabela longa                                              #
################################################################################
table_long <- table_users[,list(user=value[1],item=value[-1]),by="chunk_user"]
head(table_long)
################################################################################
#       Remodelando a tabela                                                   #
################################################################################
table_long[,value:= 1]
head(table_long)
table_wide <- reshape(data=table_long,direction="wide",idvar="user",timevar="item",v.names="value")
head(table_wide[, 1:5, with = FALSE])
################################################################################
#        Constru??o da matriz de classifica??o                                 #
################################################################################
vector_users <- table_wide[, user]
table_wide[, user:= NULL]
table_wide[, chunk_user:= NULL]
################################################################################
#   Mantendo usu?rios e itens iguais                                           #
################################################################################
setnames(x = table_wide,old = names(table_wide),new = substring(names(table_wide), 7))
#################################################################################
#Transferindo table_wide para dentro do recomendalab e identificando os usu?rios#
#################################################################################
matrix_wide <- as.matrix(table_wide)
rownames(matrix_wide) <- vector_users
head(matrix_wide[, 1:6])
################################################################################
#                   Gerando a matriz bin?ria                                   #
################################################################################
matrix_wide[is.na(matrix_wide)] <- 0
ratings_matrix <- as(matrix_wide, "binaryRatingMatrix")
ratings_matrix
image(ratings_matrix[1:50, 1:50], main = "Matriz Bin?ria de Classifica??o")
################################################################################
#    Identificando os usu?rios que compram um it?n                             #
###############################################################################
n_users <- colCounts(ratings_matrix)
qplot(n_users) + stat_bin(binwidth = 100) + ggtitle("Distribution of the number of users")
################################################################################
#                 Excluindo os outliers                                        #
################################################################################
qplot(n_users[n_users < 100]) + stat_bin(binwidth = 10) + ggtitle("Distribution of the number of users")
################################################################################
#          Definindo um m?nimo de compras                                      #
################################################################################
ratings_matrix <- ratings_matrix[, colCounts(ratings_matrix) >= 5]
ratings_matrix
################################################################################
#      Verficando se itnes excluidos n?o foram comprados                       #
################################################################################
sum(rowCounts(ratings_matrix)==0)
###############################################################################
#           Retirando os usu?rios sem item                                    #
###############################################################################
ratings_matrix <- ratings_matrix[rowCounts(ratings_matrix) >= 5, ]
ratings_matrix
################################################################################
#       Verificando itens e extraindos                                     #
###############################################################################
table_in <- data.table(table_in)
table_items <- table_in[V1 == "A"]
head(table_items)
################################################################################
#       Extraindo e renomeando a tabela                                     #
###############################################################################
table_items <- table_items[, c(2, 4, 5), with = FALSE]
setnames(table_items, 1:3, c("id", "description", "url"))
table_items <- table_items[order(id)]
head(table_items)
###############################################################################
#             Gerando a Categoria produto                                     #
###############################################################################
table_items[, category := "product"]
###############################################################################
#                       Categorizando por regi?es                             #
###############################################################################
name_countries=c(countrycode_data$country.name,"Taiwan", "UK", "Russia", "Venezuela","Slovenija", "Caribbean", "Netherlands (Holland)","Europe", "Central America", "MS North Africa")
table_items[description %in% name_countries, category := "region"]
table_items[grepl("Region", description), category := "region"]
head(table_items)
###############################################################################
#     Verificando o total de itens para cada categoria                        #
###############################################################################
table_items[, list(n_items = .N), by = category]
###############################################################################
#           Montando o Sistema de Recomenda??o                                #
###############################################################################
###############################################################################
#   Dividindo o conjunto em treino e teste                                    #
###############################################################################
which_train <- sample(x = c(TRUE, FALSE),
                      size = nrow(ratings_matrix),
                      replace = TRUE,
                      prob = c(0.8, 0.2))
recc_data_train <- ratings_matrix[which_train, ]
recc_data_test <- ratings_matrix[!which_train, ]
###############################################################################
#           Gerando a Fun??o do sistema de recomenda?a?                       #
###############################################################################
recc_model <- Recommender(data = recc_data_train,method = "IBCF",parameter = list(method = "jaccard"))
##############################################################################
#   Extranindo a matiz do slot                                               #
##############################################################################
class(recc_model@model$sim)
dim(recc_model@model$sim)
range(recc_model@model$sim)
##############################################################################
#   Visualizando a matriz de recomenda??o                                    #
##############################################################################
image(recc_model@model$sim)
##############################################################################
#      Combinando a matriz de dist?ncia com a descri??o dos itens            #
#############################################################################
dist_ratings <- as(recc_model@model$sim, "matrix")
#############################################################################
# Ajustando a matriz de itens                                               #
#############################################################################
dist_category <- table_items[, 1 - dist(category == "product")]
class(dist_category)
#############################################################################
#  Convertendo os dados brutos em matriz                                    #
#############################################################################
dist_category <- as(dist_category, "matrix")
dim(dist_category)
#############################################################################
#        Extraindo as linhas e colunas da matrix de dist?ncia               #
############################################################################
rownames(dist_category) <- table_items[, id]
colnames(dist_category) <- table_items[, id]
##############################################################################
# Extraindo as distancias dos ratings da subctegoria distancia da categoria  #
##############################################################################
vector_items <- rownames(dist_ratings)
dist_category <- dist_category[vector_items, vector_items]
##############################################################################
# Verificando se as daus matrizes s?o correspondentes                        #
##############################################################################
identical(dim(dist_category), dim(dist_ratings))
identical(rownames(dist_category), rownames(dist_ratings))
identical(colnames(dist_category), colnames(dist_ratings))
#############################################################################
#     Vendo as matrizes                                                     #
############################################################################
image(dist_category)
#############################################################################
#                Combinando as duas matrizes                                #
#############################################################################
weight_category <- 0.25
dist_tot <- dist_category * weight_category + dist_ratings * (1 - weight_category)
image(dist_tot)
#############################################################################
#Incl a matriz de reconhecimento convertendo a matriz:dist_tot em dgcmatrix #
#############################################################################
recc_model@model$sim <- as(dist_tot, "dgCMatrix")
recc_model@model$sim <- as(dist_tot, "dgCMatrix")
#############################################################################
#       Recomendar os itens                                                 #
#############################################################################
n_recommended <- 2
recc_predicted <- predict(object=recc_model,newdata = recc_data_test,n=n_recommended)
#############################################################################
#   Vendo o codido dos itens                                                #
#############################################################################
head(recc_predicted@itemLabels)
#############################################################################
#           Definindo os rotulos dos itens perdidos                         #
#############################################################################
table_labels <- data.frame(id = recc_predicted@itemLabels)
#############################################################################
#   Juntando os labels e os itens                                           #
#############################################################################
table_labels <- merge(table_labels,table_items,by="id",all.x=TRUE,all.y=FALSE,sort=FALSE)
head(table_labels)
#############################################################################
#     Converter o fator em personagem                                       #
#############################################################################
descriptions <- as(table_labels$description,"character")
head(descriptions)
#############################################################################
#                          Itens recomendados                               #
#############################################################################
recc_user_1 <- recc_predicted@items[[1]]
items_user_1 <- descriptions[recc_user_1]
head(items_user_1)
#############################################################################
#      Gerabdo a matrix de recomenda??o                                     #
#############################################################################
recc_matrix <- sapply(recc_predicted@items, function(x){
recommended <- descriptions[x]
c(recommended, rep("", n_recommended - length(recommended)))
})
dim(recc_matrix)
head(recc_matrix[,1:5])
#############################################################################
#                    Olhando as recomenda??es                              #
############################################################################
head(recc_matrix[, 1:5])
############################################################################
#                      Puxando os artigos                                  #
############################################################################
library("writexl")
artigos <- read.csv2("C:\\Users\\nonat\\Desktop\\PITCH DECK - EQUIPE 30 - dados dos projetos.csv",header=FALSE)
write_xlsx(artigos,"C:/Users/nonat/Desktop/artigos.xlsx")
head(artigos)
labels(artigos)
#############################################################################
#                       Chat Bot                                            #
#############################################################################
modelo < - lista (< font >< /font >
                    "ol?" = c (< fonte >< /font >
                                 "Ola, como vai? Por favor, indique o seu perfil." < fonte >< / fonte >
                    ) , < fonte >< /font >
                    "plataforma" = c (< fonte >< /font >
                                        "1-Pesquisador" , < fonte >< / fonte >
                                        "2-Gestor" , < fonte >< / fonte >
                                        "3-Profissional de sa?de" , < fonte >< / fonte >
                                        "4-Cidad?o" < fonte >< / fonte >
                    ) , < fonte >< /font >
                    "Gestor" = c (< fonte >< /font >
                                    "queria saber a recomenda??es de sa?de mental no SUS" < font >< /font >
                    ) , < fonte >< /font >
                    "plataforma" = c (< font >< /font >
                                        "veja os resultados dessa pesquisa" , < font >< /font >
                                        "Estudos avaliativos sobre a demanda e adequa??o da oferta de cuidado em sa?de mental no SUS" , < font >< /font >
                                        "com recomenda??o para o SUS" < font >< /font >
                                        "Atuar na forma??o de profissionais e a??es de comunica??o como v?deos e material de publicidade." < font >< /font >
                                        "Veja os detalhes da pesquisa"  
                    ) , < fonte >< /font >
                    "plataforma" = c (< font >< /font >
                                        "Essa informa??o foi ?til?" , < fonte >< / fonte >
                                        "foi ?til" , < fonte >< / fonte >
                    ) , < fonte >< /font >
                    "usu?rio" = c (< font >< /font >
                                     "1-Sim?" , < fonte >< / fonte >
                                     "2-n?o?" , < fonte >< / fonte >
                                     "plataforma" = c (< font >< /font >
                                                         "Obrigado pelo atendimento" < fonte >< / fonte >
                                                         
                                                         Al?m disso, inclu?mos algumas respostas padr?o caso nenhuma frase-chave seja encontrada:
                                                         
                                                         default_model < - c (< font >< /font >
                                                                                "Muito interessante" , < font >< /font >
                                                                                "N?o tenho certeza se entendi voc? completamente" , < font >< /font >
                                                                                "O que isso sugere para voc??" , < fonte >< / fonte >
                                                                                "Por favor, continue" , < font >< /font >
                                                                                "Continue" , ??????< font >< /font >
                                                                                "Voc? sente fortemente sobre discutir essas coisas?" < fonte >< / fonte >
                                                         )< fonte >< / fonte >
                                                         Eliza < - function ( input ) {< font >< /font > 
                                                             # corresponde a palavras-chave do modelo<font></font>
                                                             pos < - which ( lapply ( paste0 ( "(.*)?" , names ( model ) , "(.*)?" ) , grep, x = input, ignore. case = TRUE ) == 1 )< font > < /fonte >
                                                             output < - unlist ( model [ pos ])< font >< /font >
                                                             if ( comprimento ( pos ) == 0 ) {< font >< /font >  
                                                                 # escolha a resposta padr?o aleatoriamente se nenhuma palavra-chave for encontrada<font></font>
                                                                 output < - amostra ( default_model, 1 )< font >< /font >
                                                             } else {< fonte >< / / fonte >  
                                                                 # escolha a resposta aplic?vel aleatoriamente<font></font>
                                                                 pos < - ifelse ( comprimento ( pos ) > 1 , amostra ( pos , 1 ) , pos )< font >< /font >   
                                                                 output < - sample ( output, 1 )< font >< /font >
                                                                 nomes ( sa?da ) < - NULL < font >< /font > 
                                                                 # personaliza a resposta<font></font>
                                                                 tmp < - regexec ( nomes ( model )[ pos ] , input, ignore. case = TRUE )[[ 1 ]]< font >< /font >
                                                                 end_phrase < - substr ( input, start = attr ( tmp, "match.length" ) + as. numeric ( tmp ) + 1 , stop = nchar ( input )) < font >< /font >
                                                                 end_phrase < - trimws ( end_phrase, que = "certo" , whitespace = "[?!.]" )< font >< /font >
                                                                 output < - sub ( "\\$" , end_phrase, output )< font >< /font >
                                                             }< fonte >< /fonte >
                                                             sa?da < fonte >< / fonte >
                                                         }< fonte >< /fonte >  
                                                         entrada < - "" < fonte >< /font >
                                                         cat ( "Eliza: Ol?, eu sou Eliza!\n" )< font >< /font >
                                                         while ( TRUE ) {< font >< /font >  
                                                             input < - readline ( "Voc?: " )< font >< /font >
                                                             if ( input == "quit" ) break < font >< /font > 
                                                             cat ( "Eliza:" , Eliza ( entrada ))< font >< /font >
                                                         }< fonte >< /fonte >  
                                                         
# setwd("C:/Users/Rilson/Dropbox/MATE04 - Topicos em BD 20161/Atividade4/Rilson e Rodrigo")

require("plotly")
require("ggplot2")
require("plyr")
require("arules")
require("C50")
require("rpart")
require("rpart.plot")
require("rattle")

if(!require('plotly'))
  install.packages('plotly')
if(!require("ggplot2"))
  install.packages('ggplot2')
if(!require("plyr"))
  install.packages("plyr")
if(!require("arules"))
  install.packages("arules")
if(!require("C50"))
  install.packages("C50")
if(!require("rpart"))
  install.packages("rpart")
if(!require("rpart.plot"))
  install.packages("rpart.plot")
if(!require("rattle"))
  install.packages("rattle")

dt<-read.csv(file = "./database.csv", sep=";", encoding = "UTF-8")
df<-data.frame(dt)

summary(df)

##### PROBLEMA : O QUE LEVA UM PEDIDO A SER REJEITADO? #####

colnames(df)

##### REDUCTION #####
drops <- c("DATA_CADASTRO", "QTD_PEDIDOS_USUARIO", "GASTO_TOTAL_USUARIO",
           "QTD_BAIRROS_ATENDIDOS", "DETALHES_PRODUTO", "AVALIACAO", "DATA_PEDIDO",
           "PRODUTO", "COMIDA", "OPERADORA_USUARIO", "FACEBOOK",
           "DDD_USUARIO", "BAIRRO_USUARIO", "BAIRRO_ESTABELECIMENTO")

df <- df[ , !(names(df) %in% drops)]
names(df)

##### TRANSFORMATION #####
# Categorizando a data de pedido anterior como indicativo de se aquele pedido foi o primeiro
df$PEDIDO_ANTERIOR <- 'SIM'
df$PEDIDO_ANTERIOR[df$DATA_PEDIDO_ANTERIOR == "NULL"] <- 'NÃO'
# Removendo a variável original
df$DATA_PEDIDO_ANTERIOR = NULL

# Categorizando o turno do pedido
df$TURNO_PEDIDO <- substr(df$HORA_PEDIDO, 0, 2)
df$TURNO_PEDIDO <- as.numeric(sub(':', '', df$TURNO_PEDIDO))
df$TURNO_PEDIDO[df$TURNO_PEDIDO >= 4 & df$TURNO_PEDIDO < 11] <- 'MANHA'
df$TURNO_PEDIDO[df$TURNO_PEDIDO >= 11 & df$TURNO_PEDIDO < 18] <- 'TARDE'
df$TURNO_PEDIDO[df$TURNO_PEDIDO >= 18 & df$TURNO_PEDIDO <= 23] <- 'NOITE'
df$TURNO_PEDIDO[df$TURNO_PEDIDO >= 0 & df$TURNO_PEDIDO < 4] <- 'MADRUGADA'
# Removendo a variável original
df$HORA_PEDIDO <- NULL

# Usando V_ENTREGA como indicador de distância
df$V_ENTREGA_FACTOR <- cut(df$V_ENTREGA,
                           breaks = c(-1, 0, 2, 4, 6, max(df$V_ENTREGA)),
                           labels = c("Sem custo", "Custos até R$2,00", "Maior que R$2,00 e menor que R$4,00", "Maior que R$4,00 e menor que R$6,00", "Maior que R$6,00"))
df$V_ENTREGA <- NULL

names(df)

summary(df)

##### CLEANING
df_rec <- df[df$STATUS_PEDIDO == 'Recusado',]
df_clean <- df[paste(df$NOME_USUARIO, df$ESTABELECIMENTO) %in% paste(df_rec$NOME_USUARIO, df_rec$ESTABELECIMENTO),]

table(df_clean$STATUS_PEDIDO)

##### CLASSIFICATION #####
## C50
Sys.setlocale(locale="C")
ind <- sample(2, nrow(df_clean), replace=T, prob=c(0.7,0.3))
# fit model
fit <- C5.0(STATUS_PEDIDO~., data=df_clean[ind==1,], trials=10)
# summarize the fit
print(fit)
# make predictions
predictions <- predict(fit, df_clean[ind==2,])
# summarize accuracy
table(predictions, df_clean$STATUS_PEDIDO[ind==2])

str(df_clean)

library(rpart)

#dtree <- rpart(STATUS_PEDIDO ~ ., data=dt, method="class")
dtree <- rpart(STATUS_PEDIDO ~ V_ENTREGA_FACTOR + TURNO_PEDIDO + PEDIDO_ANTERIOR, data=df3, method="class")
fancyRpartPlot(dtree)

#install.packages("MASS")
library("MASS")
library("rpart")
library(neuralnet)
library(ISLR)
library("rnn")

# Carregando os dados
data <- read.csv("PETR4.SA.csv")

#Inverter a ordem das ações para pegar da última para a ´primeira
data <-data[order(data$Date, decreasing = TRUE),]

fechamento <- data$Close

fechamento_anterior <- lead(fechamento,n=1L)

data_analise <- data.frame(fechamento)
data_analise$fechamento_anterior <- fechamento_anterior

summary(data_analise)

#exclui NA
data_analise <- data_analise[1:248,]

x <- data_analise[,2]
y <- data_analise[,1]

X <- matrix(x, nrow = 31)
Y <- matrix(y, nrow = 31)

Yscaled <- (Y - min(Y)) / (max(Y) - min(Y))
Xscaled <- (X - min(X)) / (max(X) - min(X))
Y <- Yscaled
X <- Xscaled

train=1:6
test=7:8

set.seed(12)
model <- trainr(Y = Y[,train],
                X = X[,train],
                learningrate = 0.05,
                hidden_dim = 20,
                numepochs = 1000,
                network_type = "rnn"
                )


#no conjunto de treinamento
Ytrain <- t(matrix(predictr(model, X[,train]),nrow=1))
Yreal <- t(matrix(Y[,train],nrow=1))

#Percentual de variação em uma variável explicada por outra
rsq <- function(y_actual,y_predict){
  cor(y_actual,y_predict)^2
}

rsq(Yreal,Ytrain)

plot(Ytrain, type = "l", col = "darkred")
lines(Yreal, col = "darkblue", type = "l")

#no conjunto de teste
Ytest=matrix(Y[,test], nrow = 1)
Ytest = t(Ytest)
Yp <- predictr(model, Y[,test])
Ypredicted=matrix(Yp, nrow = 1)
Ypredicted=t(Ypredicted)

result_data <- data.frame(Ytest)
result_data$Ypredicted <- Ypredicted     

rsq(result_data$Ytest,result_data$Ypredicted)

mean(result_data$Ytest)
mean(result_data$Ypredicted)

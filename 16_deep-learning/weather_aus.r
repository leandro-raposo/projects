#Pacotes
library("rattle")
library("rnn")
library("ggplot2")

#Dados = Weather AUS
data(weatherAUS)
#View(weatherAUS)
#extrair somente colunas 1 and 14  e primeiras 3040 linhas (Albury location)
data <- weatherAUS[1:3040,14:15]
summary(data)

#Pre-processamento
data_cleaned <- na.omit(data)
data_used=data_cleaned[1:3000,]
x=data_used[,1]
y=data_used[,2]

Yscaled = (y - min(y)) / (max(y) - min(y))
Xscaled = (x - min(x)) / (max(x) - min(x))

y <- Yscaled
x <- Xscaled

x <- as.matrix(x)
y <- as.matrix(y)

X <- matrix(x, nrow = 30)
Y <- matrix(y, nrow = 30)

#train test split
train=1:80
test=81:100

#modelo
set.seed(12)
model <- trainr(Y = Y[,train],
                X = X[,train],
                learningrate = 0.01,
                hidden_dim = 15,
                network_type = "rnn",
                numepochs = 100)

model$error
#poucas épocas?
plot(colMeans(model$error),type='l',xlab='epoch',ylab='errors')

#predição
Yp <- predictr(model, X[,test])

#Percentual de variação em uma variável explicada por outra
#por enquanto: entenda que é um percentual de variação explicada
rsq <- function(y_actual,y_predict)
{
  cor(y_actual,y_predict)^2
}


Ytest <- matrix(Y[,test], nrow = 1)
Ytest <- t(Ytest)
Ypredicted <- matrix(Yp, nrow = 1)
Ypredicted <- t(Ypredicted)

result_data <- data.frame(Ytest)
result_data$Ypredicted <- Ypredicted     

rsq(result_data$Ytest,result_data$Ypredicted)

mean(result_data$Ytest)
mean(result_data$Ypredicted)
```

#grafico
plot(as.vector(t(result_data$Ytest)), col = 'red', type='l',
main = "Actual vs Predicted Humidity: testing set",
ylab = "Y,Yp")
lines(as.vector(t(Yp)), type = 'l', col = 'black')
legend("bottomright", c("Predicted", "Actual"),
col = c("red","black"),
lty = c(1,1), lwd = c(1,1))

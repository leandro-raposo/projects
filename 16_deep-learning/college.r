#install.packages("MASS")
library("MASS")
library("rpart")
library(neuralnet)
library(ISLR)

#Olhar os dados - data wrangling?
data <- College
# is.na(data)
# View(data)

#private = as.numeric(College$Private)-1
private <- ifelse(data$Private == 'Yes', 1, 0)

#Padronizar dados para melhor performance
data <- data[,2:18]
max_data <- apply(data, 2, max) 
min_data <- apply(data, 2, min)
scaled <- data.frame(scale(data,center = min_data, scale = max_data - min_data))

#Inclui variável explicada (target)
scaled$Private <- private

set.seed(0)
#train test split
index = sample(1:nrow(data),round(0.70*nrow(data)))
train_data <- as.data.frame(scaled[index,])
test_data <- as.data.frame(scaled[-index,])

#Utiliza o neuralnet
set.seed(0)
n = names(train_data)
f <- as.formula(paste("Private ~", paste(n[!n %in% "Private"], collapse = " + ")))
nn <- neuralnet(f,data=train_data,hidden=c(5,4),linear.output=F)
plot(nn)

pr.nn <- compute(nn,test_data[,1:17])

# Explica sapply
pr.nn$net.result <- sapply(pr.nn$net.result,round,digits=0)
pr.nn$net.result

table(test_data$Private,pr.nn$net.result)

Acc <- (62+158) / (62+158+7+6)

#CART comparação
set.seed(0)

# árvore
fit_tree <- rpart(f,method="class", data=train_data)
tree_predict <- predict(fit_tree,test_data,type = "class")
table(test_data$Private,tree_predict)

Acc_tree <- (58+159) / (58+159+11+5)

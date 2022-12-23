#install.packages("MASS")
library("MASS")
library("rpart")
library(neuralnet)

# Boston Housing Values in Suburbs of Boston
'''
# crim - per capita crime rate by town. 
# zn - proportion of residential land zoned for lots over 25,000 sq.ft. 
indus - proportion of non-retail business acres per town. 
chas - Charles River dummy variable (= 1 if tract bounds river; 0 otherwise). 
nox - nitrogen oxides concentration (parts per 10 million). 
rm - average number of rooms per dwelling. 
age - proportion of owner-occupied units built prior to 1940. 
dis - weighted mean of distances to five Boston employment centres. 
rad - index of accessibility to radial highways. 
tax - full-value property-tax rate per \$10,000. 
ptratio - pupil-teacher ratio by town. 
black - 1000(Bk − 0.63)2 where Bk is the proportion of blacks by town. 
lstat - lower status of the population (percent). 
medv - median value of owner-occupied homes in \$1000s. 
'''

set.seed(0)

#Baixa os dados
data <- Boston

#Uma olhada nos dados
head(data)

#Temos valores nulos?
data[is.na(data) == TRUE]

#Train-Test Split
train_test_split_index <- 0.8 * nrow(data)

train <- data.frame(data[1:train_test_split_index,])
test <- data.frame(data[(train_test_split_index+1): nrow(data),])

#CART

# árvore
fit_tree <- rpart(medv ~.,method="anova", data=train)
tree_predict <- predict(fit_tree,test)
mse_tree <- mean((tree_predict - test$medv)^2)

```

### NeuralNet
```{r}
#Padronizar dados para melhor performance
#Explicar apply
set.seed(0)
max_data <- apply(data, 2, max) 
min_data <- apply(data, 2, min)
scaled <- scale(data,center = min_data, scale = max_data - min_data)

index = sample(1:nrow(data),round(0.70*nrow(data)))
train_data <- as.data.frame(scaled[index,])
test_data <- as.data.frame(scaled[-index,])

#abrir o CRAN para mostrar

#Fit de neuralnet
#Executar testes com diferentes arquiteturas
nn <- neuralnet(medv~crim+zn+indus+chas+nox+rm+age+dis+rad+tax+ptratio
    +black+lstat,data=train_data,hidden=c(5,4,3,2),linear.output=T)
plot(nn)

pr.nn <- compute(nn,test_data[,1:13])
pr.nn_ <- pr.nn$net.result*(max(data$medv)-min(data$medv))+min(data$medv)
test.r <- (test_data$medv)*(max(data$medv)-min(data$medv))+min(data$medv)
MSE_nn <- mean((pr.nn_ - test.r)^2)
```

#Plot
```{r}
plot(test_data$medv,type = 'l',col="red",xlab = "x", ylab = "Valor Residencia")
lines(pr.nn$net.result,col = "blue")
```

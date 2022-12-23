#### 1 - Pacotes
library("devtools")
library(devtools)
library(RBM)
library("RBM") #install_github("TimoMatzen/RBM") https://github.com/TimoMatzen/RBM
library(dplyr)

#### 2 - Visualiza imagens
#Vamos dar uma olhada na imagem
# visualize the digits
data(MNIST)
image(matrix(MNIST$trainX[102, ], nrow = 28))

#### 3 - Treina modelo
set.seed(0)
train <- MNIST$trainX
TrainY <- MNIST$trainY

modelClassRBM <- RBM(x = train, y = TrainY, n.iter = 1000, n.hidden = 100, size.minibatch = 10)

#### 5 - Acha matriz de confusão
set.seed(0)

test <- MNIST$testX
TestY <- MNIST$testY

p <- PredictRBM(test = test, labels = TestY, model = modelClassRBM)
p

set.seed(0)
data(Fashion)

#'The labels are as follows: 
#'0: T-shirt/tops 
#'1: Trouser 
#'2: Pullover 
#'3: Dress 
#'4: Coat 
#'5: Sandal 
#'6: Shirt 
#'7: Sneaker 
#'8: Bag 
#'9: Ankle Boot 

image(matrix(Fashion$trainX[,102], nrow = 28))

Fashion$trainY[102]
Fashion$trainY[102]

#Diminui o tamanho para melhorar processamento
Fashion$trainX <- Fashion$trainX[,1:2000]
Fashion$trainY <- Fashion$trainY[1:2000]
Fashion$testX <- Fashion$testX[,1:200]
Fashion$testY <- Fashion$testY[1:200]

train <- t(Fashion$trainX)

#RBM
modelRBM <- RBM(x = train, n.iter = 1000, n.hidden = 100, size.minibatch = 10, plot = TRUE)

test <- t(Fashion$testX)

ReconstructRBM(test = test[102,], model = modelRBM)

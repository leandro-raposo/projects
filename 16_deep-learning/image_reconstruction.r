
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
image(matrix(MNIST$trainX[2, ], nrow = 28))

MNIST$trainY[2]

#### 3 - Treina modelo
set.seed(0)
train <- MNIST$trainX

modelRBM <- RBM(x = train, n.iter = 1000, n.hidden = 100, size.minibatch = 10, plot = TRUE)

#### 4 - Reconstroi a imagem
set.seed(0)
test <- MNIST$testX
testY <- MNIST$testY

test[100,]
testY[5]

ReconstructRBM(test = test[5, ], model = modelRBM)

### 5 - DBN
set.seed(0)

train <- MNIST$trainX
test <- MNIST$testX

modStack <- StackRBM(x = train, layers = c(100, 100, 100), n.iter = 1000, size.minibatch = 10)

ReconstructRBM(test = test[5, ], model = modStack, layers = 3)

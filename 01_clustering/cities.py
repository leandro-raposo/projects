########################################
#
#   CHAMANDO BIBLIOTECAS IMPORTANTES
#
########################################

import pandas as pd
import numpy as np
from sklearn.cluster import KMeans 
import arrow as ar
import matplotlib.pyplot as plt
import sklearn as sk
from sklearn.datasets import load_iris
import seaborn as sns
import tensorflow as tf
import tensorflow_datasets as tfds
import theano as th
import matplotlib as mpl
import matplotlib.pyplot as plt
import seaborn as sns
import seaborn.objects as so
import tensorflow as tf

########################################
#
#    CLUSTER NAO HIERARQUICO - Municipios
#
########################################
'''
#carregar base municipio
municipios = read.table("dados/municipios.csv", sep = ";", header = T, dec = ",")
rownames(municipios) = municipios[,1]
municipios = municipios[,-1]


#padronizar dados
municipios.padronizado = scale(municipios)

#Agora vamos rodar de 3 a 6 centros  e visualizar qual a melhor divisao
municipios.k3 = kmeans(municipios.padronizado, centers = 3)
municipios.k4 = kmeans(municipios.padronizado, centers = 4)
municipios.k5 = kmeans(municipios.padronizado, centers = 5)
municipios.k6 = kmeans(municipios.padronizado, centers = 6)

#Graficos
G1 = fviz_cluster(municipios.k3, geom = "point", data = municipios.padronizado) + ggtitle("k = 3")
G2 = fviz_cluster(municipios.k4, geom = "point",  data = municipios.padronizado) + ggtitle("k = 4")
G3 = fviz_cluster(municipios.k5, geom = "point",  data = municipios.padronizado) + ggtitle("k = 5")
G4 = fviz_cluster(municipios.k6, geom = "point",  data = municipios.padronizado) + ggtitle("k = 6")

#Criar uma matriz com 4 graficos
grid.arrange(G1, G2, G3, G4, nrow = 2)

#VERIFICANDO ELBOW 
fviz_nbclust(municipios.padronizado, FUN = hcut, method = "wss")

#juntando dados
municipios2 = read.table("dados/municipios.csv", sep = ";", header = T, dec = ",")
municipiosfit = data.frame(municipios.k6$cluster)

#Agrupar cluster e base
MunicipioFinal =  cbind(municipios2, municipiosfit)

'''

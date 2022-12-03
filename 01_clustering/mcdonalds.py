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
#     CLUSTER HIERARQUICO - MCDonald
#
########################################
'''
#Carregar base de dados: 
mcdonalds = read.table("dados/MCDONALDS.csv", sep = ";", dec = ",", header = T)
#transformar o nome dos lanches em linhas
rownames(mcdonalds) = mcdonalds[,1]
mcdonalds = mcdonalds[,-1]

#Padronizar variaveis
mcdonalds.padronizado = scale(mcdonalds)

#calcular as distancias da matriz utilizando a distancia euclidiana
distancia = dist(mcdonalds.padronizado, method = "euclidean")

#Calcular o Cluster: metodos disponiveis "average", "single", "complete" e "ward.D"
cluster.hierarquico = hclust(distancia, method = "single" )

# Dendrograma
plot(cluster.hierarquico, cex = 0.6, hang = -1)

#Criar o grafico e destacar os grupos
rect.hclust(cluster.hierarquico, k = 2)

#VERIFICANDO ELBOW 
fviz_nbclust(mcdonalds.padronizado, FUN = hcut, method = "wss")


#criando 4 grupos de lanches
grupo_lanches4 = cutree(cluster.hierarquico, k = 4)
table(grupo_lanches4)

#transformando em data frame a saida do cluster
Lanches_grupos = data.frame(grupo_lanches4)

#juntando com a base original
Base_lanches_fim = cbind(mcdonalds, Lanches_grupos)

#FAZENDO ANALISE DESCRITIVA
#MEDIAS das variaveis por grupo
mediagrupo = Base_lanches_fim %>% 
  group_by(grupo_lanches4) %>% 
  summarise(n = n(),
            Valor.Energetico = mean(Valor.Energetico), 
            Carboidratos = mean(Carboidratos), 
            Proteinas = mean(Proteinas),
            Gorduras.Totais = mean(Gorduras.Totais), 
            Gorduras.Saturadas = mean(Gorduras.Saturadas), 
            Gorduras.Trans = mean(Gorduras.Trans),
            Colesterol = mean(Colesterol), 
            Fibra.Alimentar = mean(Fibra.Alimentar), 
            Sodio = mean(Sodio),
            Calcio = mean(Calcio), 
            Ferro = mean(Ferro) )
mediagrupo


########################################
#
#    CLUSTER NAO HIERARQUICO - Mcdonald
#
########################################

#AGRUPANDO LANCHES PELO METODO NAO HIERARQUICO

#Rodar o modelo
mcdonalds.k2 = kmeans(mcdonalds.padronizado, centers = 2)

#Visualizar os clusters
fviz_cluster(mcdonalds.k2, data = mcdonalds.padronizado, main = "Cluster K2")

#Criar clusters
mcdonalds.k3 = kmeans(mcdonalds.padronizado, centers = 3)
mcdonalds.k4 = kmeans(mcdonalds.padronizado, centers = 4)
mcdonalds.k5 = kmeans(mcdonalds.padronizado, centers = 5)

#Criar graficos
G1 = fviz_cluster(mcdonalds.k2, geom = "point", data = mcdonalds.padronizado) + ggtitle("k = 2")
G2 = fviz_cluster(mcdonalds.k3, geom = "point",  data = mcdonalds.padronizado) + ggtitle("k = 3")
G3 = fviz_cluster(mcdonalds.k4, geom = "point",  data = mcdonalds.padronizado) + ggtitle("k = 4")
G4 = fviz_cluster(mcdonalds.k5, geom = "point",  data = mcdonalds.padronizado) + ggtitle("k = 5")

#Imprimir graficos na mesma tela
grid.arrange(G1, G2, G3, G4, nrow = 2)

#VERIFICANDO ELBOW 
fviz_nbclust(mcdonalds.padronizado, kmeans, method = "wss")

'''
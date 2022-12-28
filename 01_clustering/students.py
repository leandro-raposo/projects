
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from scipy.cluster.hierarchy import dendrogram, linkage
from awsLogin.awsLogin import login as al # login aws

# CLUSTER HIERARQUICO - juntos

#sync aws
client = al.login()

#LEITURA DOS DADOS
client.download_file("leandro-raposo-projects", "clustering/alunos_pap.csv", "alunos_pap.csv")
alunos_pap = open("alunos_pap.csv").readlines()
# print(alunos_pap.readlines())

df = pd.DataFrame(alunos_pap[1:], columns=['Aluno','Matematica','Portugues'])

print(alunos_pap)
print(df)

#DEFININDO O CLUSTER A PARTIR DO METODO ESCOLHIDO
#metodos disponiveis "average", "single", "complete" e "ward.D"
linkage_data = linkage(df, method='ward', metric='euclidean')
dendrogram(linkage_data)

#DESENHANDO O DENDOGRAMA
plt.show()

'''
#BRINCANDO COM O DENDOGRAMA PARA 2 GRUPOS
rect.hclust(hc4, k = 2)

#COMPARANDO DENDOGRAMAS
#comparando o metodo average com ward
dend3 = as.dendrogram(hc3)
dend4 = as.dendrogram(hc4)
dend_list = dendlist(dend3, dend4) 
#EMARANHADO, quanto menor, mais iguais os dendogramas sao
tanglegram(dend3, dend4, main = paste("Emaranhado =", round(entanglement(dend_list),2)))
#agora comparando o metodo single com complete
dend1 = as.dendrogram(hc1)
dend2 = as.dendrogram(hc2)
dend_list2 = dendlist(dend1, dend2) 
#EMARANHADO, quanto menor, mais iguais os dendogramas sao
tanglegram(dend1, dend2, main = paste("Emaranhado =", round(entanglement(dend_list2),2)))

#criando 2 grupos de alunos
grupo_alunos2 = cutree(hc4, k = 2)
table(grupo_alunos2)

#transformando em data frame a saida do cluster
alunos_grupos = data.frame(grupo_alunos2)

#juntando com a base original
Base_alunos_fim = cbind(alunos_pap, alunos_grupos)

# entendendo os clusters
#FAZENDO ANALISE DESCRITIVA
#MEDIAS das variaveis por grupo
mediagrupo_alunos = Base_alunos_fim %>% 
  group_by(grupo_alunos2) %>% 
  summarise(n = n(),
            Portugues = mean(Portugues), 
            Matematica = mean(Matematica))
mediagrupo_alunos

########################################
#
#   Brincando e comparando todos os métodos com dbscan
#
########################################

#Carregar base de dados: 
notas_categ = as.data.frame(read_excel("dados/notas_categ.xlsx"))

#pegando os dados que usaremos
notas_alunos = notas_categ %>% 
  select(Estudante, Atuaria, Mkt)

#para visualizar no plano
notas_alunos %>% ggplot() +
  geom_point(aes(x = Atuaria,
                 y = Mkt),
             size = 3)

#Transformar o nome 
rownames(notas_alunos) = notas_alunos[,1]
notas_alunos = notas_alunos[,-1]

#Padronizar variaveis
notas_alunos_pad = scale(notas_alunos)

#calcular as distancias da matriz utilizando a distancia euclidiana
distancia = dist(notas_alunos_pad, method = "euclidean")

### método hiearquico

#Calcular o Cluster
cluster.hierarquico = hclust(distancia, method = "single" )

# Dendrograma
plot(cluster.hierarquico, cex = 0.6, hang = -1)

#criando grupos
grupo_alunos_hierarquico = cutree(cluster.hierarquico, k = 3)
table(grupo_alunos_hierarquico)

#transformando em data frame a saida do cluster
grupo_alunos_hierarquico = data.frame(grupo_alunos_hierarquico)

#juntando com a base original
notas_alunos_fim = cbind(notas_alunos, grupo_alunos_hierarquico)

#visualizando em cores os clusters
notas_alunos_fim %>% ggplot() +
  geom_point(aes(x = Atuaria,
                 y = Mkt,
                 color = as.factor(grupo_alunos_hierarquico)),
             size = 3)

### método k-means

#Calcular o Cluster
cluster.k3 = kmeans(notas_alunos_pad, centers = 3)

#criando grupos
grupo_alunos_kmeans3 = data.frame(cluster.k3$cluster)

#juntando com a base original
notas_alunos_fim = cbind(notas_alunos_fim, grupo_alunos_kmeans3)

#visualizando em cores os clusters
notas_alunos_fim %>% ggplot() +
  geom_point(aes(x = Atuaria,
                 y = Mkt,
                 color = as.factor(cluster.k3.cluster)),
             size = 3)

### método dbscan

#Calcular o Cluster
dbscan = fpc::dbscan(notas_alunos_pad,eps = 0.56, MinPts = 3)

notas_alunos_fim$dbscan = dbscan$cluster

#visualizando em cores os clusters
notas_alunos_fim %>% ggplot() +
  geom_point(aes(x = Atuaria,
                 y = Mkt,
                 color = as.factor(dbscan)),
             size = 3)

'''
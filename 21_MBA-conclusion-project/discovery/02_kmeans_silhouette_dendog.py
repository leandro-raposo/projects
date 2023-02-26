import pandas as pd
from sklearn.cluster import KMeans
from scipy.cluster.hierarchy import dendrogram, linkage
from sklearn.metrics import silhouette_score
import matplotlib.pyplot as plt

# Carrega a base de dados
enem = pd.read_csv('enem.csv')

# Seleciona as colunas de interesse para o agrupamento
cols = ['NU_NOTA_CN', 'NU_NOTA_CH', 'NU_NOTA_MT', 'NU_NOTA_LC', 'NU_NOTA_REDACAO']
data = enem[cols]

# Executa o algoritmo K-means com 5 clusters
kmeans = KMeans(n_clusters=5, random_state=42).fit(data)

# Adiciona a coluna 'cluster' ao dataframe
enem['cluster'] = kmeans.labels_

# Cria o dendrograma
Z = linkage(data, 'ward')
plt.figure(figsize=(10, 5))
dendrogram(Z)
plt.show()

# Avalia a qualidade do agrupamento usando a métrica silhouette
silhouette_avg = silhouette_score(data, kmeans.labels_)
print(f'Silhouette Score: {silhouette_avg}')

# Este script carrega o arquivo enem.csv, seleciona as colunas de interesse
# (notas nas provas) e executa o algoritmo K-means com 5 clusters. Em seguida,
# adiciona a coluna cluster ao dataframe original e cria o dendrograma usando a
# função dendrogram do pacote scipy.cluster.hierarchy. Por fim, avalia a
# qualidade do agrupamento usando a métrica silhouette e imprime o resultado.
# Note que este exemplo é apenas um ponto de partida e pode ser adaptado de
# acordo com as necessidades específicas do problema.

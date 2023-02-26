import pandas as pd
import numpy as np
from sklearn.cluster import DBSCAN
from sklearn.metrics import silhouette_score

# Carrega os dados do Enem
df = pd.read_csv("dados_enem.csv")

# Seleciona as colunas que serão utilizadas para o clustering
colunas_clustering = ["NU_NOTA_CN", "NU_NOTA_CH", "NU_NOTA_MT", "NU_NOTA_REDACAO"]

# Filtra as escolas que possuem valores válidos nas colunas selecionadas
df_clustering = df.dropna(subset=colunas_clustering)

# Normaliza os dados
df_clustering_norm = (df_clustering[colunas_clustering] - df_clustering[colunas_clustering].mean()) / df_clustering[colunas_clustering].std()

# Aplica o algoritmo DBSCAN para fazer o clustering das escolas
modelo = DBSCAN(eps=0.5, min_samples=10)
df_clustering_norm["cluster"] = modelo.fit_predict(df_clustering_norm)

# Avalia os resultados utilizando a métrica de Silhouette
score = silhouette_score(df_clustering_norm[colunas_clustering], df_clustering_norm["cluster"])
print("Silhouette score: {:.3f}".format(score))

# Note que este é um exemplo básico e é necessário ajustar os parâmetros do
# DBSCAN e selecionar as colunas apropriadas para o clustering de acordo com o
# objetivo do projeto. Além disso, é importante realizar a visualização dos
# resultados obtidos e interpretar os clusters formados para obter insights
# relevantes.

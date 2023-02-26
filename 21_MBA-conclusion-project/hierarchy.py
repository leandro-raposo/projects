# Importação das bibliotecas
import pandas as pd
from sklearn.cluster import AgglomerativeClustering
from sklearn.metrics import silhouette_score

# Leitura dos dados do Enem
df_enem = pd.read_csv('dados_enem.csv')

# Seleção das colunas relevantes para o clustering
df_clustering = df_enem[['NU_NOTA_CN', 'NU_NOTA_CH', 'NU_NOTA_LC', 'NU_NOTA_MT']]

# Normalização dos dados
df_clustering = (df_clustering - df_clustering.mean()) / df_clustering.std()

# Aplicação do método hierárquico
model = AgglomerativeClustering(n_clusters=4, linkage='ward')
clusters = model.fit_predict(df_clustering)

# Cálculo do Silhouette Score
silhouette = silhouette_score(df_clustering, clusters)

# Impressão do resultado
print(f"Silhouette Score: {silhouette}")

# Nesse exemplo, é feita a leitura dos dados do Enem a partir de um arquivo
# CSV, em seguida é selecionado um conjunto de colunas relevantes para o
# clustering, normalizados e aplicado o método hierárquico com 4 clusters
# utilizando a métrica de ligação 'ward'. Por fim, é calculado o Silhouette
# Score para avaliar a qualidade da clusterização obtida. Note que, para
# obter os resultados adequados, é necessário ajustar os parâmetros do modelo
# de acordo com a natureza dos dados e do problema em questão.

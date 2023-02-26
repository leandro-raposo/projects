import pandas as pd
from sklearn.cluster import KMeans
from sklearn.metrics import silhouette_score

# Carregando a base de dados do Enem
enem = pd.read_csv('enem.csv')

# Selecionando as colunas de interesse
cols = ['NU_ANO', 'CO_MUNICIPIO', 'NU_NOTA_CN', 'NU_NOTA_CH', 'NU_NOTA_LC', 'NU_NOTA_MT']
enem = enem[cols]

# Agrupando as notas por cidade e ano
enem = enem.groupby(['NU_ANO', 'CO_MUNICIPIO']).mean().reset_index()

# Selecionando as colunas de notas
notas = ['NU_NOTA_CN', 'NU_NOTA_CH', 'NU_NOTA_LC', 'NU_NOTA_MT']
X = enem[notas]

# Normalizando as notas
X_norm = (X - X.mean()) / X.std()

# Criando o modelo de clustering com k-means
kmeans = KMeans(n_clusters=5, random_state=42)
kmeans.fit(X_norm)

# Avaliando os resultados com a métrica de silhouette
labels = kmeans.labels_
silhouette_avg = silhouette_score(X_norm, labels)

print(f'Silhouette score: {silhouette_avg}')

# Neste exemplo, a base de dados do Enem é carregada a partir do arquivo 
# "enem.csv" e as colunas de interesse são selecionadas. Em seguida, as notas
# são agrupadas por cidade e ano e as colunas de notas são selecionadas. As
# notas são normalizadas e um modelo de clustering com k-means é criado e
# ajustado aos dados. Por fim, a métrica de silhouette é calculada para
# avaliar os resultados. É importante destacar que o número de clusters e
# outras configurações do modelo devem ser ajustados de acordo com os dados e
# com os objetivos do estudo.

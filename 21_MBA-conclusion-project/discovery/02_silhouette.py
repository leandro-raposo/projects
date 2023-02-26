from sklearn.cluster import KMeans
from sklearn.datasets import make_blobs
from sklearn.metrics import silhouette_score

# Gerando dados aleatórios para exemplo
X, y = make_blobs(n_samples=1000, centers=4, n_features=5, random_state=42)

# Definindo o número de clusters para avaliar
n_clusters = [2, 3, 4, 5, 6]

# Loop para calcular o Silhouette Score para cada valor de cluster
for n in n_clusters:
    kmeans = KMeans(n_clusters=n, random_state=42)
    kmeans.fit(X)
    silhouette_avg = silhouette_score(X, kmeans.labels_)
    print("Para n_clusters =", n, "o Silhouette Score médio é", silhouette_avg)

# import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from sklearn.cluster import KMeans

# A análise do método Elbow (cotovelo) é uma técnica utilizada para identificar o número ideal de clusters em um conjunto de dados, por meio da identificação do "ponto de cotovelo" em um gráfico da soma dos quadrados das distâncias em relação aos centróides em função do número de clusters. Segue abaixo uma função Python que implementa essa técnica:

def elbow_plot(data, max_clusters=10):
    # Inicializar lista para guardar os valores de SSE
    sse = []
    
    # Executar k-means para diferentes números de clusters e guardar os valores de SSE
    for k in range(1, max_clusters+1):
        kmeans = KMeans(n_clusters=k, random_state=42).fit(data)
        sse.append(kmeans.inertia_)
    
    # Plotar gráfico de linha da soma dos quadrados das distâncias em função do número de clusters
    plt.plot(range(1, max_clusters+1), sse, 'bx-')
    plt.xlabel('Número de clusters')
    plt.ylabel('Soma dos quadrados das distâncias')
    plt.title('Método Elbow')
    plt.show()


# Essa função recebe como entrada a base de dados e o número máximo de clusters
# a serem testados. Ela executa o algoritmo k-means para diferentes números de
# clusters e guarda a soma dos quadrados das distâncias (SSE) em relação aos
# centróides para cada número de clusters. Depois, plota um gráfico de linha
# com a soma dos quadrados das distâncias em função do número de clusters,
# permitindo a visualização do ponto de cotovelo.

enem_data = pd.read_csv('enem_data.csv')
enem_grouped = enem_data.groupby('CO_MUNICIPIO_RESIDENCIA').mean().reset_index()

# Selecionar colunas com notas
notas_cols = ['NU_NOTA_CN', 'NU_NOTA_CH', 'NU_NOTA_LC', 'NU_NOTA_MT']
enem_notas = enem_grouped[notas_cols]

elbow_plot(enem_notas)

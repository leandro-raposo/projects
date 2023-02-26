import pandas as pd
# import numpy as np
from sklearn.cluster import KMeans
import matplotlib.pyplot as plt
import requests, zipfile, io

# 1. Carregar o arquivo CSV com os resultados do ENEM
# get data
url = 'https://download.inep.gov.br/microdados/microdados_enem_2021.zip'
r = requests.get(url)
z = zipfile.ZipFile(io.BytesIO(r.content))
z.extractall()

# load data
enem = pd.read_csv('DADOS/MICRODADOS_ENEM_2021.csv', sep=';', encoding='ISO-8859-1')

enem_df = pd.read_csv(enem)

# 2. Selecionar as colunas de interesse
cols = ['CO_ESCOLA_EDUCACENSO', 'SG_UF_ESC', 'NU_NOTA_CN', 'NU_NOTA_CH', 'NU_NOTA_LC', 'NU_NOTA_MT']
enem_df = enem_df[cols]

# 3. Pré-processamento dos dados
enem_df = enem_df.fillna(0)  # Preencher valores faltantes com zero
enem_df['NU_NOTA_CN'] = (enem_df['NU_NOTA_CN'] - enem_df['NU_NOTA_CN'].mean()) / enem_df['NU_NOTA_CN'].std()  # Normalização das notas
enem_df['NU_NOTA_CH'] = (enem_df['NU_NOTA_CH'] - enem_df['NU_NOTA_CH'].mean()) / enem_df['NU_NOTA_CH'].std()
enem_df['NU_NOTA_LC'] = (enem_df['NU_NOTA_LC'] - enem_df['NU_NOTA_LC'].mean()) / enem_df['NU_NOTA_LC'].std()
enem_df['NU_NOTA_MT'] = (enem_df['NU_NOTA_MT'] - enem_df['NU_NOTA_MT'].mean()) / enem_df['NU_NOTA_MT'].std()

# 4. Agrupar as escolas por estado
estados = enem_df['SG_UF_ESC'].unique()

# 5. Usar o algoritmo de clustering K-Means para agrupar as escolas de cada estado
n_clusters = 3
for estado in estados:
    estado_df = enem_df[enem_df['SG_UF_ESC'] == estado]
    escolas = estado_df['CO_ESCOLA_EDUCACENSO'].values
    notas = estado_df.iloc[:, 2:].values
    
    kmeans = KMeans(n_clusters=n_clusters, random_state=0).fit(notas)
    cluster_labels = kmeans.labels_
    
    # 6. Visualizar os resultados
    for i in range(n_clusters):
        cluster_escolas = escolas[cluster_labels == i]
        print(f'Estado: {estado}, Cluster {i+1}: {len(cluster_escolas)} escolas')

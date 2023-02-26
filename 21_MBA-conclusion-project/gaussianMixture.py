# Importando as bibliotecas necessárias
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from sklearn.mixture import GaussianMixture
from sklearn.metrics import silhouette_score

# Carregando os dados do ENEM
enem = pd.read_csv('dados_enem.csv')

# Selecionando as colunas desejadas
colunas = ['NU_NOTA_CN', 'NU_NOTA_CH', 'NU_NOTA_LC', 'NU_NOTA_MT', 'NU_NOTA_REDACAO']
enem = enem[colunas]

# Substituindo valores nulos pela média
enem = enem.fillna(enem.mean())

# Normalizando os dados
enem_norm = (enem - enem.min()) / (enem.max() - enem.min())

# Definindo o número de clusters
n_clusters = 5

# Criando o modelo GMM
gmm = GaussianMixture(n_components=n_clusters)

# Treinando o modelo
gmm.fit(enem_norm)

# Obtendo as classes dos dados
classes = gmm.predict(enem_norm)

# Calculando o score silhouette
silhouette_avg = silhouette_score(enem_norm, classes)

# Imprimindo o resultado
print("Score silhouette:", silhouette_avg)

# Nesse exemplo, é feita a leitura dos dados do ENEM, são selecionadas as
# colunas de notas, são substituídos os valores nulos pela média e os dados são
# normalizados. Em seguida, é definido o número de clusters desejado (n_clusters
# = 5) e o modelo GMM é criado e treinado com os dados normalizados. As classes
# são obtidas a partir do modelo e o score silhouette é calculado para avaliar a
# qualidade do agrupamento. Por fim, o score silhouette é impresso na tela.

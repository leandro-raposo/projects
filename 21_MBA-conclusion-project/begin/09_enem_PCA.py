import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from sklearn.decomposition import PCA
from sklearn.preprocessing import StandardScaler

# Carrega os dados do Enem em um DataFrame
df = pd.read_csv('enem.csv')

# Seleciona as colunas que serão utilizadas na análise PCA
features = ['NU_NOTA_CN', 'NU_NOTA_CH', 'NU_NOTA_MT', 'NU_NOTA_LC', 'NU_NOTA_REDACAO']
x = df.loc[:, features].values

# Normaliza os dados
x = StandardScaler().fit_transform(x)

# Aplica a análise PCA
pca = PCA(n_components=2)
principal_components = pca.fit_transform(x)
principal_df = pd.DataFrame(data=principal_components, columns=['PC1', 'PC2'])

# Plota um gráfico com as duas primeiras componentes principais
fig = plt.figure(figsize=(8,8))
ax = fig.add_subplot(1,1,1)
ax.set_xlabel('Componente Principal 1')
ax.set_ylabel('Componente Principal 2')
ax.set_title('Análise PCA')
ax.scatter(principal_df['PC1'], principal_df['PC2'], s=50)
plt.show()

# Neste exemplo, estamos utilizando as notas de ciências da natureza
# (NU_NOTA_CN), ciências humanas (NU_NOTA_CH), matemática (NU_NOTA_MT),
# linguagens e códigos (NU_NOTA_LC) e redação (NU_NOTA_REDACAO) para a análise
# PCA. Caso queira utilizar outras variáveis, basta alterar a lista features
# para incluir as colunas desejadas.

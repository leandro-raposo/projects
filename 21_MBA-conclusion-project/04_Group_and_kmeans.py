
# Selecionar as colunas relevantes: precisamos selecionar as colunas que contêm
# as notas dos alunos, a cidade e a escola de origem. No exemplo abaixo, vamos
# selecionar as colunas 'NU_NOTA_CN', 'NU_NOTA_CH', 'NU_NOTA_LC', 'NU_NOTA_MT',
# 'NU_NOTA_REDACAO', 'CIDADE' e 'NOME_ESCOLA'.

import pandas as pd

# Carregar arquivo CSV em um DataFrame
enem_escolas = pd.read_csv('enem_escolas.csv')

# Selecionar colunas relevantes
enem_notas = enem_escolas[['NU_NOTA_CN', 'NU_NOTA_CH', 'NU_NOTA_LC', 'NU_NOTA_MT', 'NU_NOTA_REDACAO', 'CIDADE', 'NOME_ESCOLA']]

# Tratar os valores ausentes: para usar o algoritmo k-means, precisamos que os
# dados estejam completos, sem valores ausentes. Uma forma simples de tratar
# esses valores é preenchê-los com a média das notas da escola. Podemos fazer
# isso usando o método fillna do pandas.

# Tratar valores ausentes
enem_notas = enem_notas.groupby(['CIDADE', 'NOME_ESCOLA']).apply(lambda x: x.fillna(x.mean()))

# Normalizar os dados: como as notas têm escalas diferentes, precisamos
# normalizá-las para que todas tenham a mesma importância no cálculo da
# distância. Uma forma comum de fazer isso é subtrair a média e dividir pelo
# desvio padrão.

# Normalizar dados
enem_norm = (enem_notas[['NU_NOTA_CN', 'NU_NOTA_CH', 'NU_NOTA_LC', 'NU_NOTA_MT', 'NU_NOTA_REDACAO']] - enem_notas[['NU_NOTA_CN', 'NU_NOTA_CH', 'NU_NOTA_LC', 'NU_NOTA_MT', 'NU_NOTA_REDACAO']].mean()) / enem_notas[['NU_NOTA_CN', 'NU_NOTA_CH', 'NU_NOTA_LC', 'NU_NOTA_MT', 'NU_NOTA_REDACAO']].std()
enem_norm['CIDADE'] = enem_notas['CIDADE']
enem_norm['NOME_ESCOLA'] = enem_notas['NOME_ESCOLA']

# Agrupar os dados por escola e cidade: agora que temos os dados normalizados,
# podemos agrupá-los por escola e cidade, para que cada grupo corresponda a uma
# escola em uma determinada cidade.

# Agrupar dados por escola e cidade
enem_grouped = enem_norm.groupby(['CIDADE', 'NOME_ESCOLA']).mean()

# Executar o algoritmo k-means: agora que temos os dados normalizados e
# agrupados, podemos executar o algoritmo k-means. Para isso, vamos usar a
# implementação do scikit-learn. Vamos escolher um número arbitrário de
# clusters, por exemplo, 3 clusters.

from sklearn.cluster import KMeans

# Definir número de clusters
n_clusters = 3

# Executar k-means
kmeans = KMeans(n_clusters=n_clusters, random_state=42).fit(enem_grouped)

# Adicionar coluna com as classes atribuídas pelo k-means
enem_grouped['kmeans_class'] = kmeans.labels_

# Visualizar os resultados: podemos visualizar os resultados do k-means em um
# gráfico scatter, onde cada escola é representada por um ponto colorido de
# acordo com a classe atribuída pelo algoritmo.

import matplotlib.pyplot as plt

# Plotar gráfico scatter
colors = ['r', 'g', 'b']
for i in range(n_clusters):
    plt.scatter(enem_grouped.loc[enem_grouped['kmeans_class'] == i]['NU_NOTA_CN'], enem_grouped.loc[enem_grouped['kmeans_class'] == i]['NU_NOTA_CH'], c=colors[i])
plt.xlabel('Nota de Ciências da Natureza')
plt.ylabel('Nota de Ciências Humanas')
plt.show()

# Esse código vai gerar um gráfico scatter que mostra a distribuição das
# escolas de acordo com as notas em Ciências da Natureza e Ciências
# Humanas, coloridas de acordo com a classe atribuída pelo k-means.

# Note que esses são apenas passos básicos para aplicar o algoritmo k-means em
# dados do ENEM. Para um projeto completo, é necessário realizar mais análises
# exploratórias, testar diferentes valores de k, avaliar a qualidade dos
# clusters gerados e interpretar os resultados obtidos.

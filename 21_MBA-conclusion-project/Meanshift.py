# Importando as bibliotecas necessárias
import pandas as pd
from sklearn.cluster import MeanShift
from sklearn.metrics import silhouette_score

# Carregando a base de dados do ENEM
df_enem = pd.read_csv('enem.csv')

# Selecionando as colunas de interesse
colunas = ['NU_ANO', 'CO_ESCOLA_EDUCACENSO', 'NU_MEDIA_CN', 'NU_MEDIA_CH', 'NU_MEDIA_LP', 'NU_MEDIA_MT', 'NU_MEDIA_RED']
df = df_enem[colunas]

# Removendo as linhas que possuem valores nulos
df.dropna(inplace=True)

# Agrupando as notas médias por escola
df_notas = df.groupby('CO_ESCOLA_EDUCACENSO').mean()

# Executando o algoritmo de Mean-Shift
modelo = MeanShift().fit(df_notas)

# Avaliando o resultado com a métrica Silhouette
labels = modelo.labels_
score = silhouette_score(df_notas, labels)

# Imprimindo o resultado
print('O score de Silhouette para o algoritmo Mean-Shift é:', score)



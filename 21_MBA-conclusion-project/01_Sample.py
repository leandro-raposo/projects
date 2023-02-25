
# Crie um código Python que receba a base analítica dos dados do enem e gere
# insights em 3 gráficos boxplot
# Para criar um código Python que receba a base analítica dos dados do Enem e
# gere insights em 3 gráficos boxplot, você pode seguir os seguintes passos:

# import
import pandas as pd
import matplotlib.pyplot as plt

# load data
# https://download.inep.gov.br/microdados/microdados_enem_2021.zip
enem = pd.read_csv('caminho/do/arquivo.csv', encoding='iso-8859-1', delimiter=';')

# select cols
enem = enem[['NU_NOTA_CN', 'NU_NOTA_CH', 'NU_NOTA_LC', 'NU_NOTA_MT', 'NU_NOTA_REDACAO']]

# remove missing values
enem.dropna(inplace=True)

# analyze
plt.figure(figsize=(10, 6))
enem[['NU_NOTA_CN']].boxplot()
plt.title('Boxplot - Nota Ciências da Natureza')

plt.figure(figsize=(10, 6))
enem[['NU_NOTA_CH']].boxplot()
plt.title('Boxplot - Nota Ciências Humanas')

plt.figure(figsize=(10, 6))
enem[['NU_NOTA_LC', 'NU_NOTA_MT', 'NU_NOTA_REDACAO']].boxplot()
plt.title('Boxplot - Notas Linguagens, Matemática e Redação')

# O código acima importa as bibliotecas pandas e matplotlib.pyplot, carrega os
# dados do Enem a partir de um arquivo CSV, seleciona as colunas relevantes
# para a análise, remove as linhas com valores faltantes e gera os 3 gráficos
# boxplot com as notas de cada área do conhecimento do Enem. É importante
# ajustar o caminho do arquivo CSV para o local onde você salvou os dados do
# Enem em seu computador.

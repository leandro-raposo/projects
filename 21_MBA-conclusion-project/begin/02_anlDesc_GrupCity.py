
import pandas as pd
import requests, zipfile, io

# get data
url = 'https://download.inep.gov.br/microdados/microdados_enem_2021.zip'
r = requests.get(url)
z = zipfile.ZipFile(io.BytesIO(r.content))
z.extractall()

# load data
enem = pd.read_csv('DADOS/MICRODADOS_ENEM_2020.csv', sep=';', encoding='ISO-8859-1')

# select cols
cols = ['CO_MUNICIPIO_RESIDENCIA', 'NU_IDADE', 'NU_NOTA_CN', 'NU_NOTA_CH', 'NU_NOTA_LC', 'NU_NOTA_MT', 'NU_NOTA_REDACAO']
enem = enem[cols]

# group by city to analyze
enem_desc = enem.groupby('CO_MUNICIPIO_RESIDENCIA').describe()

# O código acima baixa o arquivo ZIP a partir do link fornecido, extrai o
# arquivo CSV, carrega os dados em uma variável chamada "enem", seleciona as
# colunas relevantes para a análise e agrupa os dados por cidade, calculando as
# medidas descritivas para cada coluna numérica.

# Caso queira visualizar apenas as estatísticas descritivas para as notas, basta
# modificar o código no passo 4, selecionando apenas as colunas de notas:
cols = ['CO_MUNICIPIO_RESIDENCIA', 'NU_NOTA_CN', 'NU_NOTA_CH', 'NU_NOTA_LC', 'NU_NOTA_MT', 'NU_NOTA_REDACAO']
enem_notas = enem[cols]
enem_notas_desc = enem_notas.groupby('CO_MUNICIPIO_RESIDENCIA').describe()

# Note que o código pode levar algum tempo para processar, já que a base de
# dados do Enem é bastante grande. É possível que seja necessário realizar
# algumas operações de limpeza e tratamento dos dados antes de executar esses
# passos, dependendo do objetivo específico da análise.

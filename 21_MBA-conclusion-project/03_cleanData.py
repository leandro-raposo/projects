import pandas as pd

# Baixar e descompactar o arquivo ZIP
url = 'http://download.inep.gov.br/microdados/microdados_enem_2020.zip'
enem = pd.read_csv(url, compression='zip', sep=';', encoding='ISO-8859-1')

# Selecionar as colunas relevantes
cols = ['CO_MUNICIPIO_RESIDENCIA', 'NU_IDADE', 'NU_NOTA_CN', 'NU_NOTA_CH', 'NU_NOTA_LC', 'NU_NOTA_MT', 'NU_NOTA_REDACAO']
enem = enem[cols]

# Tratamento de dados: remover linhas com valores nulos
enem = enem.dropna()

# Tratamento de dados: transformar códigos de cidade em nomes
municipios = pd.read_csv('https://www.gov.br/inep/pt-br/acesso-a-informacao/dados-abertos/catalogo-de-sistemas/microdados/localidades/2020/AM_MUNICIPIOS.csv', sep=';', encoding='ISO-8859-1')
enem = pd.merge(enem, municipios[['CO_MUNICIPIO', 'NO_MUNICIPIO']], how='left', left_on='CO_MUNICIPIO_RESIDENCIA', right_on='CO_MUNICIPIO')
enem = enem.drop(['CO_MUNICIPIO_RESIDENCIA', 'CO_MUNICIPIO'], axis=1)
enem = enem.rename(columns={'NO_MUNICIPIO': 'CIDADE'})

# Tratamento de dados: remover outliers
enem = enem[(enem['NU_IDADE'] >= 0) & (enem['NU_IDADE'] <= 120)]
enem = enem[(enem['NU_NOTA_CN'] >= 0) & (enem['NU_NOTA_CN'] <= 1000)]
enem = enem[(enem['NU_NOTA_CH'] >= 0) & (enem['NU_NOTA_CH'] <= 1000)]
enem = enem[(enem['NU_NOTA_LC'] >= 0) & (enem['NU_NOTA_LC'] <= 1000)]
enem = enem[(enem['NU_NOTA_MT'] >= 0) & (enem['NU_NOTA_MT'] <= 1000)]
enem = enem[(enem['NU_NOTA_REDACAO'] >= 0) & (enem['NU_NOTA_REDACAO'] <= 1000)]

# Tratamento de dados: transformar notas em escala de 0 a 100
enem[['NU_NOTA_CN', 'NU_NOTA_CH', 'NU_NOTA_LC', 'NU_NOTA_MT']] = enem[['NU_NOTA_CN', 'NU_NOTA_CH', 'NU_NOTA_LC', 'NU_NOTA_MT']] / 10
enem['NU_NOTA_REDACAO'] = enem['NU_NOTA_REDACAO'] / 20

# Análise descritiva agrupada por cidade
enem_grouped = enem.groupby('CIDADE').describe()

# Salvar resultado em arquivo CSV
enem_grouped.to_csv('enem_descricao_por_cidade.csv')

# Neste exemplo, utilizamos o arquivo de dicionário de localidades fornecido
# pelo Inep para converter os códigos de cidade em nomes de cidade. Em seguida,
# realizamos algumas etapas de limpeza e tratamento de dados para remover
# valores nulos, outliers e transformar as notas em uma escala de 0 a 100. Por
# fim, utilizamos o método `groupby` do pandas para realizar a análise
# descritiva agrupada por cidade e salvar o resultado em um arquivo CSV.

# Vale ressaltar que, dependendo do tamanho da base de dados, a execução deste
# script pode levar alguns minutos ou até horas. É importante também que o
# computador tenha memória e capacidade de processamento suficientes para lidar
# com a quantidade de dados do Enem.

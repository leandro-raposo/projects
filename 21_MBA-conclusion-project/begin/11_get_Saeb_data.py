from zipfile import ZipFile
import requests
import pandas as pd
import io

# URLs para download dos dados do SAEB
urls = ['https://download.inep.gov.br/microdados/microdados_saeb_2021_ensino_fundamental_e_medio.zip']

# Lista para armazenar os dados
data = []

# Loop para baixar e ler os arquivos CSV
for url in urls:
    # Faz o download do arquivo zip
    response = requests.get(url)
    
    # Extrai o arquivo CSV do zip e carrega no pandas
    with ZipFile(io.BytesIO(response.content)) as zip_file:
        for csv_name in zip_file.namelist():
            if csv_name.endswith('.csv'):
                # Lê o arquivo CSV e adiciona à lista de dados
                with zip_file.open(csv_name) as csv_file:
                    data.append(pd.read_csv(csv_file, sep=';', encoding='latin1'))

# Concatena todos os dados em um único DataFrame
saeb_df = pd.concat(data, ignore_index=True)

# Exibe as primeiras linhas do DataFrame
print(saeb_df.head())

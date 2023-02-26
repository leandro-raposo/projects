import requests
import os
import pandas as pd

# Define o ano e o mês inicial e final para baixar os dados
ano_ini = 2022
ano_fim = 2022
mes_ini = 11
mes_fim = 12

# Cria a pasta para salvar os arquivos
if not os.path.exists('caged_data'):
    os.mkdir('caged_data')

# Loop pelos anos e meses para baixar os arquivos
for ano in range(ano_ini, ano_fim+1):
    for mes in range(mes_ini, mes_fim+1):
        
        # Monta o nome do arquivo
        file_name = f"CAGEDMOV{ano}{mes:02}.txt"
        
        # Monta a URL do arquivo
        url = f"https://bi.mte.gov.br/bgcaged/CagedDownload?filename={file_name}"
        
        # Define o caminho completo do arquivo a ser salvo
        file_path = os.path.join('caged_data', file_name)
        
        # Verifica se o arquivo já existe
        if os.path.exists(file_path):
            print(f"Arquivo {file_name} já existe, pulando...")
            continue
        
        # Faz o download do arquivo
        response = requests.get(url, stream=True)
        with open(file_path, 'wb') as f:
            for chunk in response.iter_content(chunk_size=1024):
                f.write(chunk)
                
        # Imprime mensagem de conclusão do download
        print(f"Arquivo {file_name} baixado com sucesso!")

# Este script baixa os arquivos de todos os meses de 2020 e 2021 e salva na pasta "caged_data" (que é criada automaticamente pelo script). Você pode alterar os valores de ano_ini, ano_fim, mes_ini e mes_fim para baixar os arquivos de outros anos/meses.

# Cada arquivo baixado contém informações sobre o emprego formal no Brasil, incluindo informações sobre as admissões e demissões de trabalhadores, segmentados por setor econômico, região geográfica, faixa etária, entre outras informações.

# agrupar por cidade

# Carrega a base de dados do CAGED
caged = pd.read_csv('caged.csv', sep=';')

# Agrupa os resultados por cidade e calcula a média do saldo de empregos
cidades = caged.groupby('município').agg({'saldo': 'mean'}).reset_index()

# Renomeia as colunas para ficar compatível com a base do ENEM
cidades = cidades.rename(columns={'município': 'Cidade', 'saldo': 'Emprego'})

# Carrega a base de dados do ENEM
enem = pd.read_csv('enem.csv')

# Agrupa os resultados por cidade e calcula a média da nota total
enem_cidades = enem.groupby('Cidade').agg({'Nota Total': 'mean'}).reset_index()

# Renomeia as colunas para ficar compatível com a base do CAGED
enem_cidades = enem_cidades.rename(columns={'Cidade': 'Cidade'})

# Faz o merge das bases de dados
dados = pd.merge(cidades, enem_cidades, on='Cidade', how='inner')

# Exibe o resultado
print(dados)

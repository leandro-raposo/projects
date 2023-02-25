import pandas as pd

'''
1- Realizar a limpeza e pré-processamento dos dados do ENEM e do PISA, como já discutido anteriormente.
2- Identificar as variáveis comuns entre as duas bases que possam ser utilizadas para correlação. No caso do ENEM e do PISA, as notas médias em cada disciplina avaliada pelo PISA e pelo ENEM podem ser utilizadas como ponto de conexão.
3- Criar um dataframe com as notas médias em cada disciplina do ENEM e do PISA, agrupadas por escola ou por município, dependendo do nível de agregação desejado.
4- Utilizar a função merge() do pandas para combinar os dataframes do ENEM e do PISA com base nas variáveis comuns identificadas anteriormente.
5- Calcular o coeficiente de correlação entre as notas médias do ENEM e do PISA em cada disciplina avaliada, utilizando a função corr() do pandas.
'''

# Carregar dados do ENEM e do PISA
enem = pd.read_csv("dados_enem.csv")
pisa = pd.read_csv("dados_pisa.csv")

# Selecionar apenas as notas médias em matemática, ciências e leitura
notas_enem = enem.groupby('CO_MUNICIPIO_RESIDENCIA')[['NU_NOTA_MT', 'NU_NOTA_CN', 'NU_NOTA_LC']].mean()
notas_pisa = pisa[pisa['LOCATION'] == 'São Paulo'].groupby('ISCED').mean()[['PV_MAT', 'PV_SCIE', 'PV_READING']]

# Renomear colunas para permitir merge
notas_enem.columns = ['MAT_ENEM', 'CIE_ENEM', 'LEIT_ENEM']
notas_pisa.columns = ['MAT_PISA', 'CIE_PISA', 'LEIT_PISA']

# Combinar os dataframes do ENEM e do PISA
notas = pd.merge(notas_enem, notas_pisa, left_index=True, right_index=True)

# Calcular a matriz de correlação entre as notas do ENEM e do PISA
correlacao = notas.corr()

# Imprimir a matriz de correlação
print(correlacao)


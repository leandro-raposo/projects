import pandas as pd
from scipy.stats import pearsonr

# leitura dos dados do Enem
enem = pd.read_csv('dados_enem.csv')

# cálculo da média de notas por escola nas provas do Enem
enem_media = enem.groupby('CO_ESCOLA')['NU_NOTA'].mean().reset_index()
enem_media.columns = ['CO_ESCOLA', 'MEDIA_ENEM']

# leitura dos dados do PISA
pisa = pd.read_csv('dados_pisa.csv')

# cálculo da média de notas por país nas provas do PISA
pisa_media = pisa.groupby('Country')['Mean'].mean().reset_index()
pisa_media.columns = ['Country', 'MEDIA_PISA']

# correlação entre as médias de notas
correlacao, p_valor = pearsonr(enem_media['MEDIA_ENEM'], pisa_media['MEDIA_PISA'])

print('Coeficiente de correlação de Pearson:', correlacao)
print('Valor p:', p_valor)

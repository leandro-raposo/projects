import pandas as pd
import matplotlib.pyplot as plt

# leitura dos dados do SAEB
saeb = pd.read_csv('saeb_data.csv')

# leitura dos dados do ENEM
enem = pd.read_csv('enem_data.csv')

# seleção das colunas com as notas do SAEB e do ENEM
saeb_notas = saeb[['cod_escola', 'nota']]
enem_notas = enem[['cod_escola', 'nota']]

# merge das notas do SAEB e do ENEM pela escola
notas = pd.merge(saeb_notas, enem_notas, on='cod_escola')

# cálculo da correlação
correlacao = notas['nota_x'].corr(notas['nota_y'])

# plot do gráfico de dispersão
plt.scatter(notas['nota_x'], notas['nota_y'])
plt.title('Correlação entre notas do SAEB e do ENEM')
plt.xlabel('Notas do SAEB')
plt.ylabel('Notas do ENEM')

# exibição da correlação no gráfico
plt.text(200, 650, f'Correlação: {correlacao:.2f}')

plt.show()

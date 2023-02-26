import pandas as pd
import numpy as np
from factor_analyzer import FactorAnalyzer
from sklearn.preprocessing import StandardScaler
import matplotlib.pyplot as plt

# Carrega o arquivo CSV com os dados do Enem
enem = pd.read_csv('enem.csv')

# Seleciona as variáveis que serão utilizadas na análise fatorial
variaveis = ['NU_NOTA_CN', 'NU_NOTA_CH', 'NU_NOTA_MT', 'NU_NOTA_LC',
             'NU_NOTA_REDACAO']

# Remove os registros que possuem pelo menos um valor NaN
enem = enem.dropna(subset=variaveis)

# Normaliza as variáveis selecionadas
scaler = StandardScaler()
enem[variaveis] = scaler.fit_transform(enem[variaveis])

# Realiza a análise fatorial
n_factors = len(variaveis)

# número de fatores a serem extraídos (pode ser menor ou igual ao número de
# variáveis)
fa = FactorAnalyzer(n_factors, rotation='varimax')
fa.fit(enem[variaveis])

# Imprime os resultados da análise
loadings = pd.DataFrame(fa.loadings_,
                        index=variaveis,
                        columns=['Factor %d' % i for i in range(1, n_factors+1)])
print('Loadings:')
print(loadings)

eigenvalues = pd.DataFrame({'Eigenvalue': fa.get_eigenvalues()},
                           index=['Factor %d' % i for i in range(1, n_factors+1)])
print('Eigenvalues:')
print(eigenvalues)

# Plota um gráfico da análise fatorial
fa.plot_scree()
plt.show()

# Neste exemplo, a biblioteca pandas é utilizada para carregar o arquivo CSV
# com os dados do Enem. Em seguida, é selecionado um conjunto de variáveis
# que serão utilizadas na análise fatorial, removendo-se os registros que
# possuem valores faltantes. As variáveis são normalizadas usando a classe
# StandardScaler da biblioteca sklearn.preprocessing.

# A análise fatorial é realizada utilizando a biblioteca factor_analyzer.
# Neste exemplo, foi utilizado o método de rotação varimax, que procura
# maximizar a variância dos carregamentos das variáveis em um único fator.
# Os resultados da análise são apresentados na saída do console, mostrando os
# carregamentos de cada variável em cada fator, bem como os autovalores.

# Por fim, um gráfico da análise fatorial é plotado utilizando o método
# plot_scree() da biblioteca factor_analyzer. Este gráfico permite visualizar a
# quantidade de variância explicada pelos diferentes fatores, permitindo
# escolher o número adequado de fatores para extrair.

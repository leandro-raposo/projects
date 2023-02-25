import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt

# Leitura dos dados de todos os anos
caged = pd.DataFrame()
for year in range(2015, 2022):
    filename = f"caged_{year}.csv"
    df = pd.read_csv(filename)
    caged = caged.append(df)

# Criação do gráfico boxplot por ano e por profissão
sns.set_theme(style="ticks")
g = sns.catplot(x="profissao", y="saldo_empregos", hue="ano", kind="box", data=caged)
g.set_xticklabels(rotation=90)
plt.show()

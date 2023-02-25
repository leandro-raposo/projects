import pandas as pd

# Ler o arquivo CSV com os dados das escolas
df_escolas = pd.read_csv("DADOS_ESCOLA_2020.CSV", sep=";", encoding="ISO-8859-1")

# Selecionar apenas as escolas públicas
df_escolas_publicas = df_escolas[df_escolas["DEPENDENCIA_ADMINISTRATIVA"] == "Federal"]

# Ordenar as escolas por ordem decrescente de média geral
df_escolas_publicas = df_escolas_publicas.sort_values("MEDIA_GERAL", ascending=False)

# Selecionar as 10 primeiras escolas
top_10_escolas_publicas = df_escolas_publicas.head(10)

# Imprimir as informações das escolas selecionadas
print(top_10_escolas_publicas[["NO_ENTIDADE", "UF_ENTIDADE", "MEDIA_GERAL"]])

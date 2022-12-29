# BIBLIOTECAS PARA ANALISE
import InstagramAPI
from instagrapi import Client
import pandas as pd
from pandas.io.json import json_normalize

# PARA QUE O JUPYTER EXIBA MAIS DE UM OUTPUT POR CELULA
from IPython.core.interactiveshell import InteractiveShell
InteractiveShell.ast_node_interactivity = "all"

# OUTRAS BIBLIOTECAS TRADICIONAIS
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
# import os
# import sys

# IGNORA ALERTAS
import warnings
warnings.filterwarnings("ignore")

#### PADRONIZACAO DE GRAFICOS E OUTPUTS ###########
sns.set()
pd.options.display.max_columns = 500
plt.style.use("fivethirtyeight")
plt.rcParams["figure.figsize"] = [10, 5]
# %load_ext nb_black

# login via API
def realiza_login(usuario, senha):
    api = InstagramAPI(usuario, senha)
    api.login()
    return api

api = realiza_login('11953702900','R@poso11')
print(f'Efetuei login no IG {api} ')
# extracao dos posts
def extrai_posts(api):
    """ EXTRAI POSTS DO PERFIL """
    my_posts = []
    has_more_posts = True
    max_id = ""

    while has_more_posts:
        api.getSelfUserFeed(maxid=max_id)
        if api.LastJson["more_available"] is not True:
            has_more_posts = False # stop condition

        max_id = api.LastJson.get("next_max_id", "")
        my_posts.extend(api.LastJson["items"])

    print("Total de posts extraídos: " + str(len(my_posts)))
    return my_posts

my_posts = extrai_posts(api)

print(f'posts extraidos {my_posts} ')

# informacoes de curtidas
def extrai_curtidas(api, meus_posts):
    """ EXTRAI AS INFORMAÇÕES DE CURTIDAS """

    likers = [] # CRIA UMA LISTA ONDE GUARDAREMOS AS CURTIDAS

    print('wait %.1f minutes' % (len(meus_posts)*2/60.))
    for i in range(len(meus_posts)):
        m_id = meus_posts[i]['id']
        api.getMediaLikers(m_id)

        likers += [api.LastJson]

        # ADICIONA ID DO POST
        likers[i]['post_id'] = m_id

    # INFORMA O TERMINO
    print('done')

    return likers

likers = extrai_curtidas(api, my_posts)

# conversao para pandas para facilitar analise
def converte_curtidas_em_df(likers):
    """ CONVERTE O DICT PARA DATAFRAME """

    df_likers = json_normalize(likers, 'users', ['post_id'])

    # CRIA UMA COLUNA PARA IDENTIFICAR TIPO DO CONTEUDO
    df_likers['content_type'] = 'like'

    return df_likers

# identificar os top20 seguidores
df_likers = converte_curtidas_em_df(api, my_posts)
df_likers.username.value_counts()[:20]

# extrair informacoes de comentatios
def extrai_curtidas(api, meus_posts):
    """ EXTRAI AS INFORMAÇÕES DE CURTIDAS """

    likers = [] # CRIA UMA LISTA ONDE GUARDAREMOS AS CURTIDAS

    print('wait %.1f minutes' % (len(meus_posts)*2/60.))
    for i in range(len(meus_posts)):
        m_id = meus_posts[i]['id']
        api.getMediaLikers(m_id)

        likers += [api.LastJson]

        # ADICIONA ID DO POST
        likers[i]['post_id'] = m_id

    # INFORMA O TERMINO
    print('done')

    return likers

likers = extrai_curtidas(api, my_posts)

def converte_comentarios_em_df(commenters):
    """ CONVERTE O DICT EM DATAFRAME """

    for i in range(len(commenters)):
        if len(commenters[i]["comments"]) > 0:
            for j in range(len(commenters[i]["comments"])):
                commenters[i]["comments"][j]["username"] = commenters[i]["comments"][j]["user"]["username"]
                commenters[i]["comments"][j]["full_name"] = commenters[i]["comments"][j]["user"]["full_name"]

    df_commenters = json_normalize(commenters, "comments", "post_id")

    return df_commenters

df_commenters = converte_comentarios_em_df(likers)

# outro tipo de grafico, por termos poucos dados
sns.countplot(
    y="username",
    data=df_commenters,
    order=df_commenters["username"].value_counts().index,
)

# dias e horarios com mais comments
df_commenters.created_at = pd.to_datetime(df_commenters.created_at, unit="s")
df_commenters.created_at_utc = pd.to_datetime(df_commenters.created_at_utc, unit="s")

df_commenters.created_at.dt.weekday.value_counts().sort_index().plot(
kind="bar",
figsize=(8, 4),
title="Comentarios por Dia da Semana (0 - Domingo, 6 - Sabado)",
)

df_commenters.created_at.dt.hour.value_counts().sort_index().plot(
kind="bar", figsize=(8, 4), title="Comentarios por Horario"
)


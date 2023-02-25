import requests
from bs4 import BeautifulSoup

# Fazer requisição para o site do TJSP
url = 'https://esaj.tjsp.jus.br/cpopg/open.do'
params = {
    'paginaConsulta': '1',
    'cbPesquisa': 'NUMPROC',
    'tipoNuProcesso': 'UNIFICADO',
    'numeroDigitoAnoUnificado': '',
    'foroNumeroUnificado': '',
    'dePesquisaNuUnificado': '',
    'dePesquisa': '',
    'tipoNuProcessoUnificado': '',
    'uuidCaptcha': '',
    'captcha': '',
    'pbEnviar': 'Pesquisar'
}
response = requests.post(url, data=params)

# Extrair informações dos processos
soup = BeautifulSoup(response.text, 'html.parser')
processos = soup.select('#tabelaUltimasMovimentacoes tr')
for processo in processos:
    numero = processo.select_one('.nuProcesso').text.strip()
    descricao = processo.select_one('.descricao').text.strip()
    data = processo.select_one('.dataHora').text.strip()
    print(numero, descricao, data)


import csv

# Salvar informações em um arquivo CSV
with open('processos.csv', 'w', newline='', encoding='utf-8') as csvfile:
    fieldnames = ['numero', 'descricao', 'data']
    writer = csv.DictWriter(csvfile, fieldnames=fieldnames)
    writer.writeheader()
    for processo in processos:
        numero = processo.select_one('.nuProcesso').text.strip()
        descricao = processo.select_one('.descricao').text.strip()
        data = processo.select_one('.dataHora').text.strip()
        if data.startswith('2022'):
            writer.writerow({'numero': numero, 'descricao': descricao, 'data': data})

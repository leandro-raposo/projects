import requests

url = "https://api-inference.huggingface.co/models/bertinho/enem-2020-scores"

headers = {
    "Authorization": "Bearer api_key"  # substituir com a chave de acesso
}

response = requests.get(url, headers=headers)

if response.status_code == 200:
    data = response.json()

    # agrupando resultados por cidade
    cities = {}
    for item in data:
        city = item["city"]
        score = item["score"]

        if city not in cities:
            cities[city] = []

        cities[city].append(score)

    # calculando a média das notas de cada cidade
    means = {}
    for city, scores in cities.items():
        mean = sum(scores) / len(scores)
        means[city] = mean

    # identificando cidades com maior distância da média das demais escolas
    avg = sum(means.values()) / len(means)
    threshold = 0.5  # definindo limite de distância da média
    outliers = [city for city, mean in means.items() if abs(mean - avg) > threshold]

    print("Cidades com maior distância da média:", outliers)
else:
    print("Erro ao acessar API:", response.text)


# Este código acessa a API do Enem 2020 e obtém as notas por cidade, agrupando
# os resultados em um dicionário. Em seguida, é calculada a média das notas de
# cada cidade e identificadas as cidades que estão a uma distância maior que
# 0.5 da média das demais escolas. Essas cidades são consideradas como tendo um
# desempenho atípico em relação às outras.

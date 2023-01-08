from countryinfo import CountryInfo

country = CountryInfo("Brazil")

print(country.alt_spellings())
# ['BR', 'Brasil', 'Federative Republic of Brazil', 'República Federativa do Brasil']

print(country.capital())
# Brasília

print(country.currencies())
# ['BRL']

print(country.languages())
# ['pt']

print(country.borders())
# ['ARG', 'BOL', 'COL', 'GUF', 'GUY', 'PRY', 'PER', 'SUR', 'URY', 'VEN']

data = country.info()
for i, j in data.items():
    print(f"{i}:>>{j}")


# O que são erros


# Trabalhando com o Try e Except com uma lista
try:
    letras = ['a', 'b', 'c']
    print(letras[3])
except IndexError:
    print("Index nao existe")

# Trabalhando com o Try e Except com o input
try:
    valor = (input("Digite o valor do produto: "))
except ValueError:
    valor = (input('Valor digitado invalido, digite o valor correto: '))

print("O valor do produto é: " + valor)

# Adicionando o Else e Finally
try:
    valor = (input("Digite o valor do produto: "))
except ValueError:
    valor = (input('Valor digitado invalido, digite o valor correto: '))
else:
    print('Valor digitado esta correto')
finally:
    ('Fim do processo')

print("O valor do produto é: " + valor)

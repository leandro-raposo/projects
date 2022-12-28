
# De Functions a Libraries

# function - usado para reutilizar a mesma funcionalidade diversas vezes no codigo
# ex: fruta; trecho de codigo que resolve um problema que acontece mais de 1x, DRY
# module - conjunto de funcoes do mesmo tema, ex: secao FLV
# package - conjunto de modulos de funcoes, ex: mercado
# library - conjunto de packages, ex: hipermercado

# Como funciona uma função
# DRY - dont repeat yourself
def boas_vindas(nome):
    print(f'Bem vindo {nome}')


boas_vindas("Leandro")

# Criando uma função de soma
def somar(x, y):
    return x + y


print(f'A soma de 5 + 2 = {somar(5, 2)}')

# Parâmetros e Argumentos em uma função
qtdade = 5
def boas_vindas(nome, notebook):
    print(f'Bem vindo {nome} temos {str(notebook)} notebooks')


boas_vindas("Leandro", qtdade)
boas_vindas("Marcos", qtdade)
boas_vindas("Linda", qtdade)

# Argumentos Default e Non-default
def boas_vindas(nome, notebook=3):
    print(f'Bem vindo {nome} temos {str(notebook)} notebooks')


boas_vindas("Leandro") # default, se passar ele sobrescreve
# default argument vem SEMPRE depois 

# Print ou Return em Funções
# funcoes realizam uma tarefa ou retornam um valor
def nome_cli(nome):
    print(f'Ola {nome}')


def novo_nome_cli(nome):
    return f'Ola {nome}'


nome_cli('Maria')
print(novo_nome_cli('Jose'))


# Vários argumentos xargs com números
def soma_total(*numeros):
    resultado = 0
    for i in numeros:
        resultado += i
    return resultado


x = soma_total(2, 3, 4, 7)
print(x)        

# Vários argumentos xargs nomeando parametros
def agencia(**carro):
    
    return carro


x = agencia(modelo='Uno', cor="Branco", motor=1.0)
print(x)

import math

# Importando um Módulo
# fatorial
fat = 4 * 3 * 2 * 1
print(fat)

print(math.factorial(4))

print(math.floor(2.7))
print(math.ceil(2.7))

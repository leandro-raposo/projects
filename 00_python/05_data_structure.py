
# Introdução a Listas
estados = ['RJ', 'SP', "MG", "BA"]
#           0     1     2     3
#           -4    -3    -2    -1

print(estados)

# Manipulando Listas
numeros = [2, 4, 6]
print(numeros)

print(estados[2])
print(estados[-1])

estados[0] = 'GO'

print(estados)

# Funções dentro de Listas
estados.append("RJ") # inclui no final da lista
print(estados)

estados.remove("RJ") # remove um item
print(estados)

estados.insert(1, "RJ") # inclui na posicao indicada
print(estados)

estados.pop(0) # remove a posicao indicada
print(estados)

estados.sort() # ordena alfabeticamente
print(estados)


# Concatenando listas
novos_estados = ['SC', 'RS', "PR"]
brasil = estados + novos_estados # concatenando lista

print(brasil)

estados.extend(novos_estados)
print(estados)

lista = ['item1', 'item2', 'item3', 'item4']
print(lista)

lista2 = [['item1', 'item2'], ['item3', 'item4']]
print(lista2)
print(lista2[0])
print(lista2[1])
print(lista2[0][0])
print(lista2[0][1])


# Extraindo variáveis de Listas
produtos = ['arroz', 'feijao', 'laranja', 'banana', 'azeitona']

item1 = produtos[0]

# item1, item2, item3, item4 = produtos
item1, item2, item3, *outros = produtos

print(item1)
print(item2)
print(item3)
print(outros)

# Looping dentro de uma lista

valores = [10, 20, 30, 40]

for x in valores:
    print(x)

for x in produtos:
    print(x)

# Verificando itens em uma lista
cores = ['amarelo', 'verde', 'azul', 'vermelho']

cor_cliente = 'roxo'

if cor_cliente in cores:
    print(f'{cor_cliente.capitalize()} em estoque')
else:
    print(f'O {cor_cliente} nao esta disponivel')

# Agregando Duas listas com o Zip
var = list('comprar') #associa cada item de uma string ao index de uma lista
print(var)

tabela_precos = zip(cores, valores)  #concatena listas associando cada item com o mesmo de sua 

print(list(tabela_precos))

print()

for i in tabela_precos:
    print(i)

# Input em uma lista
frutas_usuario = 'maca, banana, abacate'
print(frutas_usuario)

frutas_lista = frutas_usuario.split(', ')

print(frutas_lista)

# Entendendo sobre Tuples
cores_list = ['amarelo', 'verde', 'azul', 'vermelho'] # lista
cores_tup = ('amarelo', 'verde', 'azul', 'vermelho') # tupla -> immutable

print(type(cores_list))
print(type(cores_tup))

cores_list.append('preto')
# cores_tup.append('roxo')

print(cores_list)
print(cores_tup)
# tupla é mais rapida e gasta menos memoria, lista gasta mais memoria

# Trabalhando com Arrays
# quando usar array? quando sua lista for muito grande

from array import array

lista_i = [1, 2, 3, 4, 5]
lista_f = [1.1, 1.2, 1.3, 1.4]
lista_string = ['a', 'b', 'c', 'd']

print(lista_i, lista_f, lista_string)

lista_i = array('i', [1, 2, 3, 4, 5])
lista_f = array('f', lista_f)
lista_string = array('u', ['a', 'b', 'c', 'd'])

print(lista_i,'\n', lista_f,'\n', lista_string)

# Criando Sets
# set indicado para tratamento de valores duplicados
list1 = [10, 20, 30, 40, 50]
list2 = [20, 40, 60, 80]

num1 = set(list1) # sets evitam numeros duplicados, nao tem index
num2 = set(list2) # operadores union, difference, simetric, and

print(num1 | num2) # union, unifica retirando os repetidos
print()
print(num1 - num2) # difference, mostra só itens do set1 que nao estao no set2
print()
print(num1 ^ num2) # symmetric, mostra valores unicos em ambas listas
print()
print(num1 & num2) # and, mostra os duplicados

# Funções em sets
set1 = {10, 20, 30, 40, 50}
print(type(set1))

set1.add(50)
print((set1)) # nao permite duplicados

set1.update([50, 60, 70])
print((set1)) # inserção multipla

# set1.remove(80) # gera erro se o item nao existir
set1.discard(80) # permite comando de exclusao de item que nao esteja na lista
print((set1)) # inserção multipla

# Sets com strings


# Introdução a Dicionários


# Atualizando itens no dicionário


# Looping dentro de um dicionário


# Visualizando Itens, Keys e Values


# Conhecendo a Função Lambda


# Lambda dentro de uma função


# Função Map em uma lista


# Função Map com Lambda


# Função Filter


# Entendendo List Comprehension com Strings


# Entendendo List Comprehension com números


# Lista e Generator Expressions



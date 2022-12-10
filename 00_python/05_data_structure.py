
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
set1 = {'ana', 'bia', 'carol', 'dani'}
set2 = {'alberto', 'bruno', 'carlos', 'dani'}
set3 = {'AUAU', 'MIAU', 'MUuu', 'COCOCO'}

set4 = set1.union(set2) # une removendo duplicados - mesmo funcionamento de sets com
                        # strings e integers
print(set4)

set5 = set1.intersection(set2) # mantem apenas o inner
print(set5)

set6 = set3.union(set1.union(set2)) # juntando todos sets
print(set6)

set7 = (set1 | set2 | set3)
print(set7)

# Introdução a Dicionários
aluno = {'nome':'Ana', 'idade':16,'nota final':'A', 'status':True}
print(aluno['nome'])


# Atualizando itens no dicionário
aluno['nome'] = 'Jose'
print(aluno['nome'])

aluno.update({'nota final':'B'})
print(aluno['nota final'])

aluno.update({'endereco':'Av paulista, 900'})
print(aluno.get('endereco', 'Nao existe'))

del aluno['idade']
print(aluno.get('idade', 'Nao existe'))

# Looping dentro de um dicionário
for i in aluno.keys():
    print(i)

for i in aluno.items():
    print(i)

for keys, values in aluno.items():
    print(keys, values)

# Visualizando Itens, Keys e Values
aluno = {'nome':'Ana', 
         'idade':16,
         'nota final':'A', 
         'status':True, 
         'materias':['fisica', 'matematica', 'ingles']
}

print(aluno['materias'])
print(aluno.get('materias'))
print(len(aluno))
print(aluno.keys())
print(aluno.items())

# Conhecendo a Função Lambda
    # funcao pequena, sem nome
    # pode ser usada dentro de outras funcoes
    # pode ter varios argumentos e somente 1 expressao
    # deixa codigo mais clean

def soma10(x):
    return x + 10

print(soma10(10))   
    
soma_ten = lambda x: x + 10

print(soma_ten(12))

soma_complexa = lambda x, y: x + y + 10

print(soma_complexa(2, 6))

# Lambda dentro de uma função
def calcula_idade(ano, mes):
    if mes < 12: # ainda nao fez aniversario este ano
        aux = lambda x: ano
    return 2022 - aux(mes)
    
print(calcula_idade(1990, 1))

# Função Map em uma lista
lista1 = [1, 2, 3, 4]

def multX(vlr):
    return vlr * 2


lista2 = map(multX, lista1) # com funcao map aplicamos uma funcao a cada item de uma lista
print(list(lista2))

# Função Map com Lambda
print(list(map(lambda x: x * 2, lista1)))

# Função Filter
    # roda funcao dentro de uma lista (similar a map) e mostra resultado filtrado
valores = [10, 20, 30, 40, 25, 15]
def remove20(vlr):
    return vlr > 20

print(list(map(remove20, valores)))
print(list(filter(remove20, valores)))  # mostra apenas os que entraram no
                                        # criterio da funcao
# Entendendo List Comprehension com Strings
frutas1 = ['abacaxi', 'banana', 'caqui', 'damasco']
frutas2 = []

for item in frutas1:
    if 'n' in item:
        frutas2.append(item)

print(frutas1)
print(frutas2)

frutas3 = [item for item in frutas1 if 'n' in item]

print(frutas3)

# Entendendo List Comprehension com números
valores = []

for vlr in range(6):
    valores.append(vlr*10)
    
print(valores)

valores2 = [x * 10 for x in range(6)]
print(valores2)

# Lista e Generator Expressions

from sys import getsizeof

numeros1 = [x * 10 for x in range(1000000)]
print(type(numeros1))
# print(numeros1)
print(getsizeof(numeros1))

print('-----')
numeros2 = (x * 10 for x in range(1000000))
print(type(numeros2))
# print(list(numeros2))
print(getsizeof(numeros2))


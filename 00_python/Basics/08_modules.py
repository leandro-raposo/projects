
# Criando seu primeiro Modulo
def soma(a, b):
    return a + b

def mult(a, b):
    return a * b

print("A soma de 5 + 10 é: " + str(soma(5, 10)))
print("O produto de 5 * 10 é: " + str(mult(5, 10)))

# Importando um Modulo
# import myFunction
# print(myFunction.somar(5, 10))

# from myFunction import somar, multi
# print(somar(5, 10))

# Criando e Importando Packages
from package.register import cliente

cliente("Victor")

# Aplicando um Modulo
from package.findList import find_list

lista = [1, 2, 3, 5, 6]

item = 5

print(f'O {item} está na posicao {find_list(lista, 5)} do vetor')

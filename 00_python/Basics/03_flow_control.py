
# If, Else
velocidade = 100
if velocidade > 110:
    print('Devagar ai Rubinho')

velocidade = 120
if velocidade > 110:
    print('Devagar ai Rubinho')

velocidade = 100
if velocidade > 110:
    print('Devagar ai Rubinho')
else:  
    print('Velocidade OK')

velocidade = 40
if velocidade > 110:
    print('Devagar ai Rubinho')
elif velocidade <= 40:
    print('Favor dirigir acima de 80km/h')
else:  
    print('Velocidade OK')

# Operadores lógicos
renda = 5000
nome_limpo = True

if renda > 5000 and nome_limpo == True:
    print('Financiamento aprovado')
else:
    print("Financiamento negado")

renda = 15000
nome_limpo = False

if renda > 5000 or nome_limpo == True:
    print('Financiamento aprovado')
else:
    print("Financiamento negado")
        
# Operador Ternário
idade = 18

if idade >= 16:
    resultado = 'Voto permitido'
else:
    resultado = 'Voto negado'    
print(resultado)

voto = 'Voto permitido' if idade >= 16 else "Voto negado"
print(voto)

# Multiplos operadores de comparação
valor = 20

if 10 <= valor < 40:
    print("Permitido")
else:
    print("Negado")

# For Loop - Utilizando números
for numero in range(1, 6): # range (start, stop, step)
    print(numero)

# For Loop - Utilizando Strings
palavra = 'Espetacular'
for letra in palavra:
    print(f'A letra {letra} esta dentro da palavra google')

# For Loop - Utilizando If e Else
compra_sucesso = True
dados_compra = 'Compra aprovada e entrega confirmada'

for tentativas in range(3):
    if compra_sucesso:
        print('Compra confirmada e detalhes enviados para o email')
        break
else:
    print('Compra nao realizada')

# For Loop - Nested loops
# loops concatenados, loop dentro de loop
for numero in range(5): # outer loop
    print(numero)
    for segundo in range(5): # inner loop
        print(segundo)

for numero in range(1,6): # outer loop
    print('Produto ' + str(numero))
    for segundo in range(5): # inner loop
        print(numero, segundo)

# For Loop - Separando Strings
titulo = 'fantastico'
for letra in titulo:
    print(f'{letra} ' , end='')

# For Loop - Criando um Retangulo
linhas = 6
colunas = 6
simbolo = '%'

for l in range(linhas):
    for c in range(colunas):
        print(simbolo, end='')
    print()

# Conhecendo o While Loop
valor = 100
custo = 20
dia = 1
while valor > custo:
    print(f'Dia {dia} valor ${valor}')
    valor-=5 # gira o loop enquanto tenho a condicao
    dia+=1

# Diferenças entre For Loop e While Loop
# if-else - executa so 1 vez

# for - itera uma variavel, usar quando sabe a quantidade predefinida

# while - executa enquanto uma condicao for verdadeira, nao sabe o tamanho do loop

# Criando condições com While Loop
valor = int(input('Digite o valor do produto: '))

while valor > 20:
    valor *=1.1
    print(f'O valor final do seu produto sera de ${valor}')
    break




# print
print("Hello world")

# Strings e Numeros
nome = 'Leandro'
idade = 32
print(nome)
print(idade)
# Entendendo sobre Variáveis
# print("O " + nome + " tem " + idade + " anos")

# Modificando o tipo de dados
print("O " + nome + " tem " + str(idade) + " anos")

# Adicionando Input
novo_nome = input('Mas se nao é o ' + nome + " entao qual seu nome: ")
print("Nice to meet you " + novo_nome)

# Calculando a idade com o Input
nova_idade = input("Qual seu ano de nascimento: ")
nova_idade = 2022 - int(nova_idade)
print("Nossa, ta com " + str(nova_idade) + " ta véio hein")

# Entendendo o Slice
print(nome[2:5]) # index comeca em 0, indica ultimo caractere mas nao o imprime

# Utilizando Formated Strings
print(f'Nossa {novo_nome} vc ta bem mais velho que o {nome} bixao ta com {nova_idade}')

# Metodos para Strings
mensagem = 'Eu adoro comida caseira'
#           0123456789
print(mensagem.lower())

print(mensagem.upper())

print(mensagem.capitalize())

print(mensagem.find('c')) # 9

print(mensagem.find('adoro')) # 9

print(mensagem.replace('adoro', 'amo'))

print(mensagem.strip()) # remove spaces antes da string



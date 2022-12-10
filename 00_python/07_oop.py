
# Conhecendo Classes
    # utilizadas para criar objetos
    # agrupas funcoes e dados, para reutilizacao

# Criando a sua primeira classe
class Frutas:
    pass # deixa classe vazia

class Funcionario1:
    nome = 'Elena'
    sobrenome = 'Cabral'
    
usuario1 = Funcionario1()

print(usuario1)
print(usuario1.nome, usuario1.sobrenome)

# Criando Objetos dentro de uma Classe
class Funcionario2():
    pass

usuario2 = Funcionario2() # criação do objeto

# criar os parametros
usuario2.nome = 'Carol'
usuario2.sobrenome = 'Silva'
print(usuario2.nome, usuario2.sobrenome)

# Criando Construtores
class Funcionario3:
    def __init__(self, nome, sobrenome, datanasc):
        self.nome = nome
        self.sobrenome = sobrenome
        self.datanasc = datanasc

usuario3 = Funcionario3('Leandro', "Raposo", "22-1-90")
print(usuario3.nome, usuario3.sobrenome, usuario3.datanasc)
        
# Adicionando mais funções a classe
class Funcionario4:
    def __init__(self, nome, sobrenome, datanasc):
        self.nome = nome
        self.sobrenome = sobrenome
        self.datanasc = datanasc
        
    def nome_completo(self):
        return self.nome + ' ' + self.sobrenome

usuario4 = Funcionario4('Marcella', "Sposito", "23-6-92")
print(usuario4.nome_completo())
print(Funcionario4.nome_completo(usuario4))

# Calculando a idade do funcionário
from datetime import datetime

class Funcionario5:
    def __init__(self, nome, sobrenome, ano_nasc):
        self.nome = nome
        self.sobrenome = sobrenome
        self.ano_nasc = ano_nasc
        
    def nome_completo(self):
        return self.nome + ' ' + self.sobrenome

    def idade_func(self):
        ano_atual = datetime.now().year
        return int(ano_atual - self.ano_nasc)

usuario5 = Funcionario5('Marcella', "Sposito", 1992)
print(usuario5.idade_func())

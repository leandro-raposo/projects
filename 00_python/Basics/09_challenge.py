
# Uhuu! Desafios

# If Else (Desafio) - Ponto do Steak
    # rare          < 48 - selada
    # medium rare   < 54 - ao ponto para mal passada
    # medium        < 60 - ao ponto
    # medium well   < 65 - ao ponto para bem passada
    # well done     < 71 - ao ponto

temp = input('Qual o ponto da carne: ')

temp = int(temp)

if temp < 48:
    ponto = 'selada'
elif temp < 54:
    ponto = 'ao ponto para mal passada'
elif temp < 60:
    ponto = 'ao ponto'
elif temp < 75:
    ponto = 'ao ponto para bem passada'
else:
    ponto = 'ao ponto'

    print(f'A {temp} *C a carne está {ponto}')

# Funções (Desafio) - Calculadora para Pintura

class Tinta:
    def __init__(self, rendimento, altura, largura):
        self.rendimento = rendimento
        self.altura = altura
        self.largura = largura
        self.area = altura * largura

    def calc_rendimento(self):
        return self.area / self.rendimento

    # entradas
rendimento = int(input('Qual o rendimento da tinta: '))
altura = int(input('Qual a altura da parede: '))
largura = int(input('Qual o largura da parede: '))

# main
pintura = Tinta(rendimento, altura, largura)
qtd_latas = pintura.calc_rendimento()

# output
print(f"Para pintar uma area de {pintura.area} voce precisara de {qtd_latas} latas de tinta")

# Sets (Desafio) - Filtrando funcionários em uma empresa
funcionarios = ['Ana', "Beatriz", "Caio", "Daniel"]
turno_dia = ['Ana', 'Beatriz']
turno_noite = ['Caio', 'Daniel']
tem_carro = ['Beatriz', 'Caio']

# tem carro e trabalha noite
carro_dia = set(turno_dia).intersection(tem_carro)
print(f'Funcionarios que tem carro e trabalham a noite {carro_dia}')
# tem carro e trabalha dia
carro_noite = set(turno_noite).intersection(tem_carro)
print(f'Funcionarios que tem carro e trabalham a noite {carro_noite}')

# nao tem carro
nao_tem = set(funcionarios).difference(tem_carro)
print(f'Funcionarios que tem carro e trabalham a noite {nao_tem}')

# If Elif (Desafio) - Calculo de BMI

def calc_imc(peso, altura):
    return peso / (altura * altura)

def define_titulo(imc):
    if imc < 18.5:
        return 'magreza'
    elif imc < 24.9:
        return 'normal'
    elif imc < 29.9:
        return 'sobrepeso'
    elif imc < 39.9:
        return 'obesidade'
    else:
        return 'obesidade grave'
    

peso = float(input(f'Qual seu peso: '))
altura = float(input(f'Qual seu altura: '))
imc = int(calc_imc(peso, altura))

print(f'Com esta altura, pesando {peso} kg seu IMC é {imc} voce esta com peso {define_titulo(imc)} ')

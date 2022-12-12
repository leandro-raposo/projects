
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


# If Elif (Desafio) - Calculo de BMI



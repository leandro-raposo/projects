
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


# Sets (Desafio) - Filtrando funcionários em uma empresa


# If Elif (Desafio) - Calculo de BMI



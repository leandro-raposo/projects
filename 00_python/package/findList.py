
def find_list(list[], item):
    for i, valor in enumerate(len(list[])):
        if i == item:
            print("entrei no if")
            retorno = i
        else:
            retorno = -1
    return retorno

lista = [1, 2, 3, 5, 6]

print(find_list(lista, 5))

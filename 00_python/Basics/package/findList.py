
def find_list(list, item):
    for i, valor in enumerate(list):
        if valor == item:
            return i
    return -1

def firstUniqueChar(s):
    from collections import Counter
    count = Counter(s)
    for i , j in enumerate(s):
        if count[j] == 1:
            return i
    else:
        return -1

param = "amankharwal"
print(f'The 1st unique letter is \'{param[firstUniqueChar(param)]}\' on position {firstUniqueChar(param)+1} ' )

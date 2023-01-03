from collections import defaultdict

def group_anagrams(a):
    dfdict = defaultdict(list)
    for i in a:
        sorted_i = " ".join(sorted(i))
        dfdict[sorted_i].append(i)
    return dfdict.values()

# creating a list of words containing anagrams and some other words:

words = ["tea", "eat", "bat", "ate", "arc", "car"]

print(group_anagrams(words))

# dict_values([['tea', 'eat', 'ate'], ['bat'], ['arc', 'car']])


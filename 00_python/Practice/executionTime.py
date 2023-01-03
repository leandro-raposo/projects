
from time import time

start = time()

# Counting
for i in range(10000):
    print(i)

# Calculating time
end = time()
execution_time = end - start
print("Execution Time : ", execution_time)

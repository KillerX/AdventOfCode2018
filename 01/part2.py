from time import sleep

f = open('input.txt')

lines = [int(l.strip()) for l in f.readlines()]

print(lines)

ll = len(lines)
i = 0
current = 0

freqs = {}

while True:
    if i >= ll:
        print('.')
        i = 0
    x = current + lines[i]
    current = x

    if x in freqs:
        print(x)
        break

    freqs[x] = True
    i += 1

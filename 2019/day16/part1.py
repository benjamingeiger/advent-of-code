from itertools import cycle

digits = [int(x) for x in open("input.txt", "r").readline().strip()]
# digits = [1, 2, 3, 4, 5, 6, 7, 8]

def makepattern(pos):
    for _ in range(pos):
        yield 0

    while True:
        for _ in range(pos + 1):
            yield 1
        for _ in range(pos + 1):
            yield 0
        for _ in range(pos + 1):
            yield -1
        for _ in range(pos + 1):
            yield 0

def fftphase(digits):
    outdigits = []
    for i, d in enumerate(digits):
        vs = list(zip(digits, makepattern(i)))
        outdigits.append(abs(sum(x * y for x, y in vs)) % 10)
    return outdigits

for _ in range(100):
    digits = fftphase(digits)

print("".join(str(d) for d in digits))

from fractions import Fraction
from math import gcd

with open("input.txt", "r") as f:
    lines = f.readlines()

asteroids = []
for i, row in enumerate(lines):
    for j, col in enumerate(row):
        if col == '#':
            asteroids.append((i, j))

print(asteroids)

def process(x, y):
    if x == y == 0: return (0, 0)
    d = gcd(abs(x), abs(y))
    # print(x, y, d, x//d, y//d)
    return (x // d, y // d)

tmpbase = None
tmpbasecount = 0
for baseX, baseY in asteroids:
    print(baseX, baseY)
    angles = set()
    for otherX, otherY in asteroids:
        # print(otherX, otherY, baseX, baseY)
        print(otherX, otherY, process(otherY - baseY, otherX - baseX))
        angles.add(process(otherY - baseY, otherX - baseX))

    print(len(angles))
    if len(angles) > tmpbasecount:
        tmpbasecount = len(angles)
        tmpbase = (baseX, baseY)

print(tmpbase)
print(tmpbasecount)

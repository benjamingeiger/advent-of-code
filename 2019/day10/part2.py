from fractions import Fraction
from math import gcd, atan2, pi
from collections import defaultdict

with open("input.txt", "r") as f:
    lines = f.readlines()

asteroids = []
for i, row in enumerate(lines):
    for j, col in enumerate(row):
        if col == '#':
            asteroids.append((j, -i))

print(asteroids)

directions = defaultdict(list)
baseX, baseY = 26, -36
for otherX, otherY in asteroids:
    if otherX == baseX and otherY == baseY: continue
    # angle = atan2(otherY - baseY, otherX - baseX)
    angle = atan2(otherX - baseX, otherY - baseY)
    if angle < 0:
        angle += 2 * pi
    directions[angle].append((otherX, otherY))

print(directions)

angles = directions.keys()
print(list(sorted(angles)))

destroyed = []
for angle in angles:
    directions[angle] = sorted(directions[angle], key=lambda p: p[0] - baseX + p[1] - baseY)

keeplooping = True
while keeplooping:
    keeplooping = False
    for angle in sorted(angles):
        print(angle)
        if len(directions[angle]) > 0:
            keeplooping = True
            destroyed.append(directions[angle][0])
            directions[angle] = directions[angle][1:]

print(destroyed)
print(destroyed[199])

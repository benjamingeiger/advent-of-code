from intcode import IntCodeComputer
from collections import defaultdict

code = [int(x) for x in open("input.txt", "r").readline().split(",")]

points = defaultdict(lambda: defaultdict(lambda: None))
def findpoint(r, c):
    if points[r][c] is not None:
        return points[r][c]
    droid = IntCodeComputer(code[:])
    values = droid.run([r, c])
    return values[-1]

size = 99
curr = 0
curc = 0

step = 128
sourcer = 0
sourcec = 0
while True:
    curr += step
    while findpoint(curr, curc) == 0:
        curc += 1
    print(f"testing {curr-size}, {curc}")
    if findpoint(curr - size, curc + size) == 1:
        # potential match, back up
        if step == 1:
            print(f"MATCH: {curr - size}, {curc}")
            sourcer, sourcec = curr - size, curc
            break
        else:
            curr -= step
            curc -= step
            step //= 2

for r in range(sourcer - 3, sourcer + 3):
    for c in range(sourcec - 3, sourcec + 3):
        print(f"{r},{c}: {findpoint(r + size, c)}, {findpoint(r, c + size)}")

print(f"possible match: {curr}, {curc}")

state = tuple(r.strip() for r in open("input.txt", "r").readlines()[:5])

def liveneighbors(state, x, y):
    live = 0

    if x > 0 and state[x-1][y] == '#':
        live += 1
    if x < 4 and state[x+1][y] == '#':
        live += 1
    if y > 0 and state[x][y-1] == '#':
        live += 1
    if y < 4 and state[x][y+1] == '#':
        live += 1

    return live

def step(state):
    newstate = [[False for _ in range(5)] for _ in range(5)]
    for x in range(5):
        for y in range(5):
            live = liveneighbors(state, x, y)
            if live == 1:
                newstate[x][y] = True
            elif live == 2 and state[x][y] == '.':
                newstate[x][y] = True
            else:
                newstate[x][y] = False
    return tuple("".join("#" if c else "." for c in line) for line in newstate)

print(step(("....#", "#..#.", "#..##", "..#..", "#....")))

def biodiversity(state):
    count = "".join(state)
    total = 0
    for i, c in enumerate(count):
        if c == '#':
            total += 2**i

    return total

history = set()
while state not in history:
    history.add(state)
    state = step(state)

print(state)
print(biodiversity(state))

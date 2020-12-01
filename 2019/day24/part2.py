from collections import defaultdict

state = defaultdict(lambda: (".....",".....",".....",".....","....."))
state[0] = tuple(r.strip() for r in open("input.txt", "r").readlines()[:5])
# state[0] = tuple(("....#", "#..#.", "#.?##", "..#..", "#...."))
toplevel = 0
bottomlevel = 0

def liveneighbors(state, level, r, c):
    if (r, c) == (2, 2):
        return 0

    live = 0

    uplevel = state[level - 1]
    downlevel = state[level + 1]

    if r == 0:
        # upper edge, go out
        if uplevel[1][2] == '#': live += 1
    elif r == 4:
        # bottom edge, go out
        if uplevel[3][2] == '#': live += 1

    if c == 0:
        # left edge, go out
        if uplevel[2][1] == '#': live += 1
    elif c == 4:
        # right edge, go out
        if uplevel[2][3] == '#': live += 1

    if (r, c) == (1, 2):
        # upper edge, go in
        live += sum(1 for x in range(5) if downlevel[0][x] == '#')
    elif (r, c) == (3, 2):
        # lower edge, go in
        live += sum(1 for x in range(5) if downlevel[4][x] == '#')
    elif (r, c) == (2, 1):
        # left edge, go in
        live += sum(1 for x in range(5) if downlevel[x][0] == '#')
    elif (r, c) == (2, 3):
        # right edge, go in
        live += sum(1 for x in range(5) if downlevel[x][4] == '#')

    if r > 0 and (r, c) != (3, 2) and state[level][r-1][c] == '#':
        live += 1
    if r < 4 and (r, c) != (1, 2) and state[level][r+1][c] == '#':
        live += 1
    if c > 0 and (r, c) != (2, 3) and state[level][r][c-1] == '#':
        live += 1
    if c < 4 and (r, c) != (2, 1) and state[level][r][c+1] == '#':
        live += 1

    print(f"{level}, {r}, {c}: {live}")
    return live

def step(state):
    global bottomlevel
    global toplevel
    newstate = defaultdict(lambda: [[False for _ in range(5)] for _ in range(5)])
    for level in range(bottomlevel - 1, toplevel + 2):
        print(f"iterating level {level}")
        for x in range(5):
            for y in range(5):
                live = liveneighbors(state, level, x, y)
                if live == 1:
                    newstate[level][x][y] = True
                    if level < bottomlevel:
                        bottomlevel -= 1
                    elif level > toplevel:
                        toplevel += 1
                elif live == 2 and state[level][x][y] == '.':
                    newstate[level][x][y] = True
                    if level < bottomlevel:
                        bottomlevel -= 1
                    elif level > toplevel:
                        toplevel += 1
                else:
                    newstate[level][x][y] = False
    
    returnstate = defaultdict(lambda: (".....",".....",".....",".....","....."))
    for level in range(bottomlevel, toplevel + 1):
        returnstate[level] = tuple("".join("#" if c else "." for c in line) for line in newstate[level])

    return returnstate

def dump(state, bottomlevel, toplevel):
    for level in range(bottomlevel, toplevel + 1):
        print(f"Depth {level}:")
        print("\n".join(state[level]))
        print()

print(state)
for _ in range(200):
    state = step(state)

dump(state, bottomlevel, toplevel)
total = 0
for level in range(bottomlevel, toplevel + 1):
    for row in state[level]:
        for cell in row:
            if cell == '#':
                total += 1
print(total)

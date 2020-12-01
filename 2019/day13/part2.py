from collections import defaultdict
from intcode import IntCodeComputer

def divide_chunks(l, n): 
    for i in range(0, len(l), n):  
        yield l[i:i + n] 

def visualize(screen):
    image = []
    for y in range(25):
        line = []
        for x in range(50):
            if screen[(x, y)] == 0:
                line.append(" ")
            elif screen[(x, y)] == 1:
                line.append("#")
            elif screen[(x, y)] == 2:
                line.append("@")
            elif screen[(x, y)] == 3:
                line.append("=")
            elif screen[(x, y)] == 4:
                line.append("*")

        image.append("".join(line))
    return "\n".join(image)

infile = open("input2.txt", "r")
code = [int(x) for x in infile.readline().split(",")]

screen = defaultdict(int)
score = 0
prevballpos = 0
ballpos = 17 
balldir = 0
paddlepos = 0
game = IntCodeComputer(code)

while not game.halted:
    print("JOYSTICK POS:", balldir)
    output = game.run([0])

    prevballpos = ballpos
    for x, y, tile in divide_chunks(output, 3):
        if (x, y) == (-1, 0):
            score = tile
        else:
            screen[(x, y)] = tile

        if tile == 4:
            ballpos = x
        elif tile == 3:
            paddlepos = x

    if ballpos > prevballpos:
        balldir = 1
    elif ballpos < prevballpos:
        balldir = -1
    else:
        balldir = 0
    print("BALL:", prevballpos, ballpos, balldir)

    print(visualize(screen))

print(score)

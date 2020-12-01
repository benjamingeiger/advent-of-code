from collections import defaultdict
from intcode import IntCodeComputer

infile = open("input.txt", "r")
code = [int(x) for x in infile.readline().split(",")]

game = IntCodeComputer(code)

output = game.run([])

def divide_chunks(l, n): 
      
    # looping till length l 
    for i in range(0, len(l), n):  
        yield l[i:i + n] 

screen = defaultdict(int)
for x, y, tile in divide_chunks(output, 3):
    screen[(x, y)] = tile

count = 0
for (x, y), tile in screen.items():
    if tile == 2:
        print(x, y)
        count += 1

print(count)

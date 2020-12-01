from intcode import IntCodeComputer
from collections import defaultdict
import sys

code = [int(x) for x in open("input.txt", "r").readline().split(",")]

computers = [IntCodeComputer(code[:]) for _ in range(50)]

switch = defaultdict(list)

firstrun = True
while True:
    for compidx, computer in enumerate(computers):
        if firstrun:
            output = computer.run([compidx, -1])
        else:
            output = computer.run(switch[compidx] + [-1])
            switch[compidx] = []

        print(compidx)
        for i in range(len(output) // 3):
            dest = output[i*3]
            x = output[i*3 + 1]
            y = output[i*3 + 2]
            
            print("---", dest, x, y)

            switch[dest].append(x)
            switch[dest].append(y)

        if len(switch[255]) > 0:
            print(switch[255])
            sys.exit()
    firstrun = False


from intcode import IntCodeComputer
from collections import defaultdict
import sys

code = [int(x) for x in open("input.txt", "r").readline().split(",")]

computers = [IntCodeComputer(code[:]) for _ in range(50)]

switch = defaultdict(list)

nat = (None, None)
prevnat = (None, None)

firstrun = True
while True:
    idle = True
    for compidx, computer in enumerate(computers):
        if firstrun:
            output = computer.run([compidx, -1])
        else:
            output = computer.run(switch[compidx] + [-1])
            switch[compidx] = []

        # print(compidx)
        for i in range(len(output) // 3):
            dest = output[i*3]
            x = output[i*3 + 1]
            y = output[i*3 + 2]
            
            print("---", compidx, dest, x, y)

            if dest == 255:
                nat = (x, y)

            switch[dest].append(x)
            switch[dest].append(y)

            idle = False

    if idle:
        if nat == prevnat:
            print(nat)
            sys.exit()
        else:
            print(f"=== NAT extended: {x}, {y}")
            switch[0].extend(nat)
            prevnat = nat

    firstrun = False


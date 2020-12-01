from sys import exit
from math import gcd

initialpositions = [(3, 15, 8, 0, 0, 0), (5, -1, -2, 0, 0, 0), (-10, 8, 2, 0, 0, 0), (8, 4, -5, 0, 0, 0)]

# initialpositions = [(-1,0,2,0,0,0), (2,-10,-7,0,0,0), (4,-8,8,0,0,0), (3,5,-1,0,0,0)]

xvalues = [(x[0], x[3]) for x in initialpositions]
yvalues = [(x[1], x[4]) for x in initialpositions]
zvalues = [(x[2], x[5]) for x in initialpositions]

def cycletime(dimstate):
    states = set()

    curstate = tuple(dimstate[:])
    i = 0
    while curstate not in states:
        # print(curstate)
        states.add(curstate)
        i += 1
        if i % 1000 == 0:
            print(i)

        newstate = []
        for pos, vel in curstate:
            dvel = 0
            for posp, _ in curstate:
                if posp > pos:
                    dvel += 1
                elif posp < pos:
                    dvel -= 1

            vel += dvel
            pos += vel
            newstate.append((pos, vel))

        curstate = tuple(newstate)

    return len(states)

xcycle = cycletime(xvalues)
print(xcycle)
ycycle = cycletime(yvalues)
print(ycycle)
zcycle = cycletime(zvalues)
print(zcycle)

def lcm(x, y):
    return x * y // gcd(x, y)

print(xcycle, ycycle, zcycle, lcm(xcycle, lcm(ycycle, zcycle)))

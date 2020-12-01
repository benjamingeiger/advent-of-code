from collections import defaultdict

def getorbits():
    with open("input.txt", "r") as f:
        lines = f.readlines()

        orbits = defaultdict(list)
        for line in lines:
            orbits[line[:3]].append(line[4:7])

        return orbits

orbits = getorbits()
print(orbits)
print(orbits["COM"])

def countorbits(orbits):

    totalorbits = 0

    queue = [("COM", 0)]
    done = []
    while len(queue) > 0:
        cur, dist = queue.pop()
        queue.extend([(p, dist + 1) for p in orbits[cur]])
        done.append(cur)

        totalorbits += dist

    return totalorbits

print(countorbits(orbits))

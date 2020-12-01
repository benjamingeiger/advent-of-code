from collections import defaultdict

def getorbits():
    with open("input.txt", "r") as f:
        lines = f.readlines()

        orbits = {}
        for line in lines:
            orbits[line[4:7]] = line[0:3]

        print(len(orbits))

        return orbits

orbits = getorbits()

def findpathtoroot(orbits, start):
    cur = orbits[start]
    path = [cur]

    while cur != "COM":
        cur = orbits[cur]
        path.append(cur)

    print(path)
    return path

youpath = findpathtoroot(orbits, "YOU")
sanpath = findpathtoroot(orbits, "SAN")

print(len(set(youpath).symmetric_difference(set(sanpath))))



# def counttransfers(orbits):


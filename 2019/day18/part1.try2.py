import networkx as nx
from networkx.algorithms.shortest_paths.generic import shortest_path
from networkx.algorithms.cycles import cycle_basis

import heapq

mazetxt = open("input.txt", "r").readlines()
# mazetxt = [
    # "#################",
    # "#i.G..c...e..H.p#",
    # "########.########",
    # "#j.A..b...f..D.o#",
    # "########@########",
    # "#k.E..a...g..B.n#",
    # "########.########",
    # "#l.F..d...h..C.m#",
    # "#################"]

G = nx.Graph()

keys = {}
gates = {}
start = None

for r, row in enumerate(mazetxt):
    for c, cell in enumerate(row):
        if cell == "#":
            continue
        G.add_node((r, c))
        if 'a' <= cell <= 'z':
            keys[cell] = (r, c)
        elif 'A' <= cell <= 'Z':
            gates[(r, c)] = cell.lower()
        elif cell == '@':
            keys[cell] = (r, c)
            start = (r, c)
        elif cell == ".":
            pass

# print(G.nodes())
for (r, c) in G.nodes():
    if (r, c + 1) in G.nodes():
        G.add_edge((r, c), (r, c + 1))
    if (r + 1, c) in G.nodes():
        G.add_edge((r, c), (r + 1, c))

# print(keys)
# print(gates)
# print(start)

# print(G.edges())

paths = {}
distances = {}

for k1 in keys:
    for k2 in keys:
        if (k1, k2) not in paths:
            path = shortest_path(G, keys[k1], keys[k2])
            paths[(k1, k2)] = list(path)
            paths[(k2, k1)] = list(reversed(path))
            distances[(k1, k2)] = distances[(k2, k1)] = len(path) - 1


for p in paths:
    print(paths[p])

keysneeded = {}
for k in keys:
    path = shortest_path(G, start, keys[k])
    keysneeded[k] = set(gates[(r, c)] for (r, c) in path if (r, c) in gates)
    keysneeded[k] = keysneeded[k].union(set(k for k in keys if keys[k] in path)).difference(set([k]))
    print(k, keysneeded[k])

# distance, sequence, keys
initialstate = (0, 0, ['@'], ['@'])

reachable_cache = {}
def reachable(state):
    _, path, keys = state
    # print(state)
    keylist = tuple(sorted(keys))
    if (path[-1], keylist) in reachable_cache:
        return reachable_cache[(path[-1], keylist)]

    reachable_cache[(path[-1], keylist)] = set(k for k in keysneeded if keysneeded[k] <= set(keys)) - set(keys)
    return reachable_cache[(path[-1], keylist)]

numkeys = len(keys)
mindist = min(x for x in distances.values() if x > 0)
print(numkeys, mindist)

states = [initialstate]
heapq.heapify(states)
# print(states)
i = 0
shortestlen = 7062
shortestpath = None
while True:
    try:
        heuristic, distance, path, mykeys = heapq.heappop(states)
    except IndexError:
        print(shortestlen, shortestpath)
        break

    i += 1
    if i % 1000000 == 0:
        print(shortestlen, heuristic, distance, path, mykeys)
    if distance > shortestlen:
        continue
    if len(mykeys) == numkeys:
        # print(distance)
        if distance < shortestlen:
            shortestlen = distance
            shortestpath = path
    for step in reachable((distance, path, mykeys)):
        newdistance = distance + distances[(step, path[-1])]
        if newdistance > shortestlen:
            continue
        newheuristic = newdistance + (mindist * (numkeys - len(mykeys)))
        newpath = path[:]
        newpath.append(step)
        newkeys = mykeys[:]
        newkeys.append(step)
        heapq.heappush(states, (newheuristic, newdistance, newpath, newkeys))


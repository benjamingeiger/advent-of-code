import networkx as nx
from networkx.algorithms.shortest_paths.generic import shortest_path
from networkx.algorithms.cycles import cycle_basis

import heapq

# mazetxt = open("input.txt", "r").readlines()
mazetxt = [
    "#################",
    "#i.G..c...e..H.p#",
    "########.########",
    "#j.A..b...f..D.o#",
    "########@########",
    "#k.E..a...g..B.n#",
    "########.########",
    "#l.F..d...h..C.m#",
    "#################"]

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


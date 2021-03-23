import networkx as nx
from networkx.algorithms.shortest_paths.generic import shortest_path
from networkx.algorithms.cycles import cycle_basis

mazetxt = open("input.txt", "r").readlines()

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

for k1 in keys:
    for k2 in keys:
        if (k1, k2) not in paths:
            path = shortest_path(G, keys[k1], keys[k2])
            paths[(k1, k2)] = list(path)
            paths[(k2, k1)] = list(reversed(path))

for p in paths:
    print(paths[p])

keysneeded = {}
for k in keys:
    path = shortest_path(G, start, keys[k])
    keysneeded[k] = set(gates[(r, c)] for (r, c) in path if (r, c) in gates)
    print(k, keysneeded[k])


class State(object):
    def __init__(self, keysneeded):
        self.keysneeded = keysneeded
        self.allkeys = set(self.keysneeded.keys())

        self.keys = ['@']
        self.distance = 0

    def options(self):
        # print(self.allkeys)
        # print(self.keys)
        toget = self.allkeys.difference(self.keys)

        unreachable = set(x for x in self.keysneeded if self.keysneeded[x].issuperset(self.keys))
        return toget.difference(unreachable)

    def done(self):
        return len(self.options()) == 0

    def moveto(self, newcur):
        # print(f"moving from {self.keys[-1]} to {newcur}")
        newstate = State(self.keysneeded)
        newstate.keys = self.keys[:] + [newcur]
        # print(self.keys)
        # print(newcur)
        # print(newstate.keys)
        newstate.distance = self.distance + len(paths[(self.keys[-1], newcur)]) - 1

        return newstate

start = State(keysneeded)
# print(start.keys)
# print(start.options())

active = [start.moveto(o) for o in start.options()]

while len(active) > 0:
    print(f"active states: {len(active)}")
    states = active[:]
    active = []
    for s in states:
        active.extend(s.moveto(o) for o in s.options())

print(done)

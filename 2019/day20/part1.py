from collections import defaultdict
import networkx as nx
from networkx.algorithms.shortest_paths.generic import shortest_path

mazetxt = open("input.txt", "r").readlines()

G = nx.Graph()
for r, row in enumerate(mazetxt):
    for c, cell in enumerate(row):
        if cell == ".":
            G.add_node((r, c))

for (r, c) in G.nodes():
    if (r + 1, c) in G.nodes():
        G.add_edge((r, c), (r + 1, c))
    if (r, c + 1) in G.nodes():
        G.add_edge((r, c), (r, c + 1))

warps = defaultdict(list)
for node in G.nodes():
    if G.degree(node) == 1:
        r, c = node
        if 'A' <= mazetxt[r - 2][c] <= 'Z':
            warps[f"{mazetxt[r-2][c]}{mazetxt[r-1][c]}"].append((r, c))
        elif 'A' <= mazetxt[r + 2][c] <= 'Z':
            warps[f"{mazetxt[r+1][c]}{mazetxt[r+2][c]}"].append((r, c))
        elif 'A' <= mazetxt[r][c - 2] <= 'Z':
            warps[f"{mazetxt[r][c-2]}{mazetxt[r][c-1]}"].append((r, c))
        elif 'A' <= mazetxt[r][c + 2] <= 'Z':
            warps[f"{mazetxt[r][c+1]}{mazetxt[r][c+2]}"].append((r, c))
        else:
            print(f"warning, spurious end at {r},{c}")

print(warps)
for name, endpoints in warps.items():
    if len(endpoints) != 2:
        print(f"warning: bad warp {name}: {endpoints}")
    else:
        G.add_edge(*endpoints)

print(len(shortest_path(G, warps['AA'][0], warps['ZZ'][0])) - 1)

from collections import defaultdict
import networkx as nx
from networkx.algorithms.shortest_paths.generic import shortest_path

mazetxt = open("input.txt", "r").readlines()

G = nx.Graph()
cells = []
for r, row in enumerate(mazetxt):
    for c, cell in enumerate(row):
        if cell == ".":
            cells.append((r, c))

nrows = len(mazetxt)
ncols = max(len(r) for r in mazetxt)

edges = []
for (r, c) in cells:
    if (r + 1, c) in cells:
        edges.append(((r, c), (r + 1, c)))
    if (r, c + 1) in cells:
        edges.append(((r, c), (r, c + 1)))

print(cells, edges)

outerwarps = {}
innerwarps = {}
tmpG = nx.Graph()
tmpG.add_nodes_from(cells)
tmpG.add_edges_from(edges)
for node in tmpG.nodes():
    print(node)
    if tmpG.degree(node) == 1:
        r, c = node
        if r <= 5 or r >= nrows - 5 or c <= 5 or c >= ncols - 5:
            if 'A' <= mazetxt[r - 2][c] <= 'Z':
                outerwarps[f"{mazetxt[r-2][c]}{mazetxt[r-1][c]}"] = (r, c)
            elif 'A' <= mazetxt[r + 2][c] <= 'Z':
                outerwarps[f"{mazetxt[r+1][c]}{mazetxt[r+2][c]}"] = (r, c)
            elif 'A' <= mazetxt[r][c - 2] <= 'Z':
                outerwarps[f"{mazetxt[r][c-2]}{mazetxt[r][c-1]}"] = (r, c)
            elif 'A' <= mazetxt[r][c + 2] <= 'Z':
                outerwarps[f"{mazetxt[r][c+1]}{mazetxt[r][c+2]}"] = (r, c)
            else:
                print(f"warning, spurious end at {r},{c}")
        else:
            if 'A' <= mazetxt[r - 2][c] <= 'Z':
                innerwarps[f"{mazetxt[r-2][c]}{mazetxt[r-1][c]}"] = (r, c)
            elif 'A' <= mazetxt[r + 2][c] <= 'Z':
                innerwarps[f"{mazetxt[r+1][c]}{mazetxt[r+2][c]}"] = (r, c)
            elif 'A' <= mazetxt[r][c - 2] <= 'Z':
                innerwarps[f"{mazetxt[r][c-2]}{mazetxt[r][c-1]}"] = (r, c)
            elif 'A' <= mazetxt[r][c + 2] <= 'Z':
                innerwarps[f"{mazetxt[r][c+1]}{mazetxt[r][c+2]}"] = (r, c)
            else:
                print(f"warning, spurious end at {r},{c}")


print(outerwarps)
print(innerwarps)


for level in range(100):
    for (r1, c1) in cells:
        G.add_node((level, r1, c1))

    for ((r1, c1), (r2, c2)) in edges:
        G.add_edge((level, r1, c1), (level, r2, c2))

    for warp, (r1, c1) in innerwarps.items():
        (r2, c2) = outerwarps[warp]
        G.add_edge((level, r1, c1), (level + 1, r2, c2))

r1, c1 = outerwarps['AA']
r2, c2 = outerwarps['ZZ']

print(len(shortest_path(G, (0, r1, c1), (0, r2, c2))) - 1)

from collections import defaultdict
import networkx as nx
from networkx.algorithms.shortest_paths.generic import shortest_path
from networkx.algorithms.shortest_paths.unweighted import single_source_shortest_path
from intcode import IntCodeComputer

code = [int(x) for x in open("input.txt", "r").readline().split(",")]

NORTH = 1
SOUTH = 2
WEST = 3
EAST = 4

def move(pos, direction):
    # print("trying to move" + str(direction))
    if direction == NORTH:
        return pos[0] - 1, pos[1]
    elif direction == SOUTH:
        return pos[0] + 1, pos[1]
    elif direction == WEST:
        return pos[0], pos[1] - 1
    elif direction == EAST:
        return pos[0], pos[1] + 1
    else:
        print("invalid direction", direction)
        return pos

reverse = {NORTH: SOUTH, SOUTH: NORTH, WEST: EAST, EAST: WEST}

pos = (0, 0)
target = None

grid = defaultdict(lambda: " ")
gridnet = nx.Graph()

grid[pos] = "."
gridnet.add_node((0, 0))
breadcrumbs = []
complete = False

droid = IntCodeComputer(code)

droid.run([])
while not complete and not droid.halted:
    # print(pos)
    if grid[move(pos, NORTH)] == " ":
        # print(f"attempting to move north from {pos}")
        output = droid.run([NORTH])[-1]
        # print(output)
        if output == 0:
            grid[move(pos, NORTH)] = "#"
        elif output > 0:
            breadcrumbs.append((reverse[NORTH], pos))
            gridnet.add_node(move(pos, NORTH))
            gridnet.add_edge(pos, move(pos, NORTH))
            pos = move(pos, NORTH)
            grid[pos] = "."
            if output == 2:
                target = pos
                grid[pos] = "@"
    elif grid[move(pos, SOUTH)] == " ":
        # print(f"attempting to move south from {pos}")
        output = droid.run([SOUTH])[-1]
        # print(output)
        if output == 0:
            grid[move(pos, SOUTH)] = "#"
        elif output > 0:
            breadcrumbs.append((reverse[SOUTH], pos))
            gridnet.add_node(move(pos, SOUTH))
            gridnet.add_edge(pos, move(pos, SOUTH))
            pos = move(pos, SOUTH)
            grid[pos] = "."
            if output == 2:
                target = pos
    elif grid[move(pos, WEST)] == " ":
        # print(f"attempting to move west from {pos}")
        output = droid.run([WEST])[-1]
        # print(output)
        if output == 0:
            grid[move(pos, WEST)] = "#"
        elif output > 0:
            breadcrumbs.append((reverse[WEST], pos))
            gridnet.add_node(move(pos, WEST))
            gridnet.add_edge(pos, move(pos, WEST))
            pos = move(pos, WEST)
            grid[pos] = "."
            if output == 2:
                target = pos
    elif grid[move(pos, EAST)] == " ":
        # print(f"attempting to move east from {pos}")
        output = droid.run([EAST])[-1]
        # print(output)
        if output == 0:
            grid[move(pos, EAST)] = "#"
        elif output > 0:
            breadcrumbs.append((reverse[EAST], pos))
            gridnet.add_node(move(pos, EAST))
            gridnet.add_edge(pos, move(pos, EAST))
            pos = move(pos, EAST)
            grid[pos] = "."
            if output == 2:
                target = pos
    else:
        # backtrack
        if len(breadcrumbs) == 0:
            # we've hit everything
            complete = True
            break
        direction, newpos = breadcrumbs.pop()
        # print(f"attempting to backtrack to {newpos}")
        output = droid.run([direction])
        if direction > 0:
            pos = newpos

    # print(grid)

gridpoints = grid.keys()
minrow = min(x[0] for x in gridpoints)
maxrow = max(x[0] for x in gridpoints)
mincol = min(x[1] for x in gridpoints)
maxcol = max(x[1] for x in gridpoints)

grid[target] = "^"

for row in range(minrow, maxrow + 1):
    for col in range(mincol, maxcol + 1):
        print(grid[(row, col)], end="")
    print()

print(target)

print(shortest_path(gridnet, (0, 0), target))
print(len(shortest_path(gridnet, (0, 0), target)))
paths = single_source_shortest_path(gridnet, target)
print(max(len(x) for x in paths.values()))

from intcode import IntCodeComputer

code = [int(x) for x in open("input.txt", "r").readline().split(",")]

camera = IntCodeComputer(code)

output = camera.run([])

image = ("".join(chr(x) for x in output)).split()
print("\n".join(image))

points = []
for r_, line in enumerate(image[1:-1]):
    r = r_ + 1
    for c_, cell in enumerate(line[1:-1]):
        c = c_ + 1
        if cell != '#': continue
        if image[r - 1][c] == '#' and image[r + 1][c] == '#' and image[r][c + 1] == '#' and image[r][c - 1] == '#':
            points.append((r, c))

print(points)
print(sum(x * y for x, y in points))

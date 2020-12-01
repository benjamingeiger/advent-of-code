from intcode import IntCodeComputer

code = [int(x) for x in open("input.txt", "r").readline().split(",")]


total = 0
for x in range(50):
    for y in range(50):
        droid = IntCodeComputer(code[:])
        values = droid.run([x, y])
        print(x, y, values)
        total += values[-1]

print(total)


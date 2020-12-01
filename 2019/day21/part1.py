from intcode import IntCodeComputer

droidcode = [int(x) for x in open("input.txt", "r").readline().split(",")]
droid = IntCodeComputer(droidcode)

def inputify(lines):
    output = []
    for line in lines:
        for char in line:
            output.append(ord(char))
        output.append(10)

    return output

def outputify(codes):
    output = []
    for char in codes:
        if 0 <= char <= 255:
            output.append(chr(char))
        else:
            output.extend(outputify(inputify([str(char)])))
    return "".join(output)

springcode = [
    "NOT C T",
    "AND D T",
    "NOT A J",
    "OR T J",
    "WALK"
]

output = droid.run(inputify(springcode))
print(outputify(output))

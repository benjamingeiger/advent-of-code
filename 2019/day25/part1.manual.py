from intcode import IntCodeComputer

code = [int(x) for x in open("input.txt", "r").readline().split(",")]
game = IntCodeComputer(code)

def asciify(chars):
    return "".join(chr(x) for x in chars)
def charify(text):
    return [ord(x) for x in text] + [10]

line = ""
while not game.halted:
    output = game.run(charify(line))
    print(asciify(output))
    line = input(">").strip()

print("game halted")

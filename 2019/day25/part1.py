from functools import reduce
from intcode import IntCodeComputer

code = [int(x) for x in open("input.txt", "r").readline().split(",")]

def asciify(chars):
    return "".join(chr(x) for x in chars)
def charify(text):
    return [ord(x) for x in text] + [10]

def powerset(lst):
    return reduce(lambda result, x: result + [subset + [x] for subset in result], lst, [[]])

opposites = {"north": "south", "south": "north", "west": "east", "east": "west"}

paths = {}
paths['cake'] = ["south"]
paths['mutex'] = paths['cake'] + ["south", "west"]
paths['klein bottle'] = ["west"]
paths['monolith'] = paths['klein bottle'] + ["south", "east"]
paths['fuel cell'] = paths['monolith'] + ["south"]
paths['astrolabe'] = paths['fuel cell'] + ["west", "west"]
paths['tambourine'] = paths['klein bottle'] + ["west", "north"]
paths['dark matter'] = paths['klein bottle'] + ["west", "west"]

def collect(item):
    steps = paths[item][:]
    steps.append(f"take {item}")
    for s in reversed(paths[item]):
        steps.append(opposites[s])

    return steps

for items in powerset(paths.keys()):
    game = IntCodeComputer(code)

    steps = []
    for item in items:
        steps.extend(collect(item))
    steps.extend(["west", "west", "west", "west", "north"])

    for step in steps:
        output = game.run([])
        print(asciify(output))
        print(f"> {step}")
        output = game.run(charify(step))
        print(asciify(output))
    if not game.halted:
        print(f"SOLUTION FAILED: {items}")
        continue
    else:
        print(f"SOLUTION SUCCESS: {items}")
        break

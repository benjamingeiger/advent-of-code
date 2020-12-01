from intcode import IntCodeComputer

prog = open("input.txt", "r").readline().split(",")
state = [int(x) for x in prog]

compy = IntCodeComputer(state)
compy.run()

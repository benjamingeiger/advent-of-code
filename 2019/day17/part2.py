from intcode import IntCodeComputer

code = [int(x) for x in open("input.txt", "r").readline().split(",")]
code[0] = 2

vacuum = IntCodeComputer(code)

mainprog = "A,A,B,C,C,A,B,C,A,B\n"
funcA = "L,12,L,12,R,12\n"
funcB = "L,8,L,8,R,12,L,8,L,8\n"
funcC = "L,10,R,8,R,12\n"

routines = [ord(x) for x in mainprog + funcA + funcB + funcC + "n\n"]

output = vacuum.run(routines)

print(output)

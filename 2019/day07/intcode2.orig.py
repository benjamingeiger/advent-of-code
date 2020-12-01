class IntCodeComputer (object):

    def __init__(self, state):
        self.ram = state
        self.ip = 0
        self.suspended = True
        self.halted = False

        self.input = []
        self.output = []

    def run(self, input_=[]):
        self.input.extend(input_[:])

        self.suspended = False

        while not (self.suspended or self.halted):
            self.tick()

        return self.output

    def get_input(self):
        if len(self.input) > 0:
            return self.input.pop(0)
        else:
            self.ip -= 2
            self.suspended = True

    def do_output(self, value):
        self.output.append(value)
        print("******* OUTPUT:", value)

    def tick(self):
        instruction, modes = self.get_opcode()

        if instruction == 1: # add
            r1 = self.fetch_argument(modes[0])
            r2 = self.fetch_argument(modes[1])
            wa = self.fetch_argument(modes[2], write=True)
            self.update(wa, r1 + r2)
        elif instruction == 2: # multiply
            r1 = self.fetch_argument(modes[0])
            r2 = self.fetch_argument(modes[1])
            wa = self.fetch_argument(modes[2], write=True)
            self.update(wa, r1 * r2)
        elif instruction == 3: # input
            wa = self.fetch_argument(modes[0], write=True)
            newval = self.get_input()
            if newval is not None:
                self.update(wa, newval)
        elif instruction == 4: # output
            r1 = self.fetch_argument(modes[0])
            self.do_output(r1)
        elif instruction == 5: # jump if true
            c = self.fetch_argument(modes[0])
            t = self.fetch_argument(modes[1])
            if c != 0:
                self.ip = t
        elif instruction == 6: # jump if false
            c = self.fetch_argument(modes[0])
            t = self.fetch_argument(modes[1])
            if c == 0:
                self.ip = t
        elif instruction == 7: # less than
            r1 = self.fetch_argument(modes[0])
            r2 = self.fetch_argument(modes[1])
            wa = self.fetch_argument(modes[2], write=True)
            if r1 < r2:
                self.update(wa, 1)
            else:
                self.update(wa, 0)
        elif instruction == 8: # equals
            r1 = self.fetch_argument(modes[0])
            r2 = self.fetch_argument(modes[1])
            wa = self.fetch_argument(modes[2], write=True)
            if r1 == r2:
                self.update(wa, 1)
            else:
                self.update(wa, 0)
        elif instruction == 99: # halt
            self.halted = True
            print("hlt")
        else: # unknown instruction
            self.halted = True
            print("Unknown instruction", instruction)
            return

    def get_opcode(self):
        val = self.ram[self.ip]
        # print(type(val))

        opcode = val % 100
        val //= 100

        modes = []

        while val > 0:
            modes.append(val % 10)
            val //= 10

        modes.extend([0] * (3 - len(modes)))

        # print("IP =", self.ip)
        self.ip += 1

        # print("fetch opcode", opcode, modes)
        return opcode, modes

    def fetch_argument(self, mode=0, write=False):
        addr = self.ram[self.ip]
        
        if write:
            value = addr
        elif mode == 0:
            value = self.ram[addr]
        elif mode == 1:
            value = addr
        else:
            print("invalid mode", mode)

        self.ip += 1

        # print("fetch arg", value, "(at", addr, ")")
        return value

    def update(self, addr, value):
        # print("update", addr, value)
        self.ram[addr] = value

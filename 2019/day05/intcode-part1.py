class IntCodeComputer (object):

    def __init__(self, state):
        self.ram = state
        self.ip = 0
        self.running = False

    def run(self):
        self.running = True

        while self.running:
            self.tick()

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
            newval = int(input(""))
            self.update(wa, newval)
        elif instruction == 4: # output
            r1 = self.fetch_argument(modes[0])
            print("**** OUTPUT:", r1)
        elif instruction == 99: # halt
            self.running = False
            print("hlt")
        else: # unknown instruction
            self.running = False
            print("Unknown instruction", instruction)
            return

    def get_opcode(self):
        val = self.ram[self.ip]

        opcode = val % 100
        val //= 100

        modes = []

        while val > 0:
            modes.append(val % 10)
            val //= 10

        modes.extend([0] * (3 - len(modes)))

        print("IP =", self.ip)
        self.ip += 1

        print("fetch opcode", opcode, modes)
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

        print("fetch arg", value, "(at", addr, ")")
        return value

    def update(self, addr, value):
        print("update", addr, value)
        self.ram[addr] = value

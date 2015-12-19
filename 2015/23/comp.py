#!/usr/bin/python

"""
Module-specific doc string.
"""

from __future__ import absolute_import
from __future__ import division
from __future__ import unicode_literals
from __future__ import print_function

import sys
if sys.version_info < (3, 0):
    input = raw_input

#regs = {'a': 0, 'b': 0} # Original conditions.
regs = {'a': 1, 'b': 0} # Part 2.
pc = 0

def main():
    global pc, regs
    with open("input.txt", "r") as f:
        lines = f.readlines()

    # I was going to parse the lines into memory and then run it there,
    # but it was easier just to parse each line as I needed it.
    while pc < len(lines):
        line = lines[pc]
        print("PC:", pc, line)
        print(regs)
        tokens = line.split()
        if tokens[0].lower() == 'hlf':
            reg = tokens[1].lower()
            regs[reg] //= 2
            pc += 1
        elif tokens[0].lower() == 'tpl':
            reg = tokens[1].lower()
            regs[reg] *= 3
            pc += 1
        elif tokens[0].lower() == 'inc':
            reg = tokens[1].lower()
            regs[reg] += 1
            pc += 1
        elif tokens[0].lower() == 'jmp':
            dst = int(tokens[1])
            pc += dst
        elif tokens[0].lower() == 'jie':
            reg = tokens[1][:-1]
            dst = int(tokens[2])
            if regs[reg] % 2 == 0:
                pc += dst
            else:
                pc += 1
        elif tokens[0].lower() == 'jio':
            reg = tokens[1][:-1]
            dst = int(tokens[2])
            if regs[reg] == 1:
                pc += dst
            else:
                pc += 1
        else:
            print("Unknown instruction:", line)
            break

    print()
    print("ANSWER:", regs['b'])

if __name__ == "__main__":
    main()

# vim: set et sw=4 ts=4:

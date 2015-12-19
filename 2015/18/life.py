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

#import numpy as np
#import scipy as sp
#import matplotlib.pyplot as plt
#import pandas as pd

def neighbors(state, r, c):
    if r == 0:
        rs = (r, r + 1)
    elif r == 99:
        rs = (r - 1, r)
    else:
        rs = (r - 1, r, r + 1)

    if c == 0:
        cs = (c, c + 1)
    elif c == 99:
        cs = (c - 1, c)
    else:
        cs = (c - 1, c, c + 1)

    total = 0
    for x in rs:
        for y in cs:
            if (x != r or y != c) and state[(x, y)]:
                total += 1
    return total

def step(state):
    newstate = {}
    for r in range(100):
        for c in range(100):
            n = neighbors(state, r, c)
            if n == 2:
                newstate[(r, c)] = state[(r, c)]
            elif n == 3:
                newstate[(r, c)] = True
            else:
                newstate[(r, c)] = False

    newstate[(0, 0)] = True
    newstate[(0, 99)] = True
    newstate[(99, 0)] = True
    newstate[(99, 99)] = True

    return newstate

def main():
    with open("input.txt", "r") as f:
        lines = f.readlines()

    state = {}
    for r, line in enumerate(lines):
        for c, ch in enumerate(line):
            if ch == ".":
                state[(r, c)] = False
            elif ch == "#":
                state[(r, c)] = True

    for _ in range(100):
        state = step(state)

    print(sum(1 for r, c in state if state[(r, c)]))




if __name__ == "__main__":
    main()

# vim: set et sw=4 ts=4:

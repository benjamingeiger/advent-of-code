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

def isinteger(s):
    return all(x in ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9'] for x in s)

wires = {}
memo = {}

def evaluate(w):
    if w in memo:
        return memo[w]

    # if it's a constant, evaluate it
    if type(w) == int or isinteger(w):
        return int(w)
    
    tokens = wires[w]
    if len(tokens) == 1:
        return evaluate(tokens[0])

    if len(tokens) == 2 and tokens[0].upper() == 'NOT':
        tmp = evaluate(tokens[1])
        return (~tmp & 0xFFFF)

    # from here on out they're all 3-arg

    if tokens[1].upper() == 'AND':
        tmp1 = evaluate(tokens[0])
        tmp2 = evaluate(tokens[2])
        memo[w] = tmp1 & tmp2
        return tmp1 & tmp2

    if tokens[1].upper() == 'OR':
        tmp1 = evaluate(tokens[0])
        tmp2 = evaluate(tokens[2])
        memo[w] = tmp1 | tmp2
        return tmp1 | tmp2

    if tokens[1].upper() == 'LSHIFT':
        tmp1 = evaluate(tokens[0])
        tmp2 = int(tokens[2])
        memo[w] = (tmp1 << tmp2) & 0xFFFF
        return (tmp1 << tmp2) & 0xFFFF

    if tokens[1].upper() == 'RSHIFT':
        tmp1 = evaluate(tokens[0])
        tmp2 = int(tokens[2])
        memo[w] = (tmp1 >> tmp2) & 0xFFFF
        return (tmp1 >> tmp2) & 0xFFFF

    raise Exception("Unimplemented operation")


def main():
    with open("input.txt", "r") as f:
        lines = f.readlines()

    for line in lines:
        tokens = line.split()
        dest = tokens[-1]
        wires[dest] = tokens[:-2]

    print(evaluate('a'))

if __name__ == "__main__":
    main()

# vim: set et sw=4 ts=4:

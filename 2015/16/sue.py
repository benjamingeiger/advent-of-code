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

#children: 3
#cats: 7
#samoyeds: 2
#pomeranians: 3
#akitas: 0
#vizslas: 0
#goldfish: 5
#trees: 3
#cars: 2
#perfumes: 1

def verify(attribute, value):
    data = {"children": 3,
            "cats": 7,
            "samoyeds": 2,
            "pomeranians": 3,
            "akitas": 0,
            "vizslas": 0,
            "goldfish": 5,
            "trees": 3,
            "cars": 2,
            "perfumes": 1}

    if attribute in ("cats", "trees"):
        return value > data[attribute]
    elif attribute in ("pomeranians", "goldfish"):
        return value < data[attribute]
    else:
        return value == data[attribute]

def main():
    with open("input.txt", "r") as f:
        lines = f.readlines()

    for line in lines:
        tokens = line.split()
        number = tokens[1][:-1]
       
        
        if not verify(tokens[2][:-1], int(tokens[3][:-1])):
            #print("Mismatched {}".format(tokens[2][:-1]))
            continue
        if not verify(tokens[4][:-1], int(tokens[5][:-1])):
            #print("Mismatched {}".format(tokens[4][:-1]))
            continue
        if not verify(tokens[6][:-1], int(tokens[7])):
            #print("Mismatched {}".format(tokens[6][:-1]))
            continue

        print("possible match: {}".format(number))

if __name__ == "__main__":
    main()

# vim: set et sw=4 ts=4:

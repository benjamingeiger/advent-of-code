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

def distance(speed, burst, rest, time):
    cycles = time // (burst + rest)
    distance = speed * burst * cycles
    leftover = time % (burst + rest)
    if leftover > burst:
        distance += burst * speed
    else:
        distance += leftover * speed

    return distance

def main():
    with open("input.txt", "r") as f:
        lines = f.readlines()

    reindeer = {}
    for line in lines:
        tokens = line.split()
        name = tokens[0]
        speed = int(tokens[3])
        burst = int(tokens[6])
        rest = int(tokens[13])
        reindeer[name] = (speed, burst, rest)

    time = 2503
    for r in reindeer:
        speed, burst, rest = reindeer[r]
        print(r, distance(speed, burst, rest, time))

if __name__ == "__main__":
    main()

# vim: set et sw=4 ts=4:

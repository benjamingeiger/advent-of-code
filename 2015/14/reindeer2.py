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

def race(reindeer, time):
    points = {r: 0 for r in reindeer}
    position = {r: 0 for r in reindeer}

    def tick(t):
        for r in reindeer:
            speed, burst, rest = reindeer[r]
            if t % (burst + rest) < burst:
                position[r] += speed

        leaderpos = max(position.values())
        for r in position:
            if position[r] == leaderpos:
                points[r] += 1

    for t in range(time):
        tick(t)

    return max(points.values())

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
    print(race(reindeer, time))


if __name__ == "__main__":
    main()

# vim: set et sw=4 ts=4:

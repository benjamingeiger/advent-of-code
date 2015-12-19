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

from collections import defaultdict



def main():
    data = sys.stdin.read(10000)

    presents = defaultdict(int)
    presents[(0, 0)] = 1

    x = 0
    y = 0

    for ch in data:
        if ch not in ('^', 'v', '<', '>'):
            continue

        if ch == '^':
            y += 1
        if ch == 'v':
            y -= 1
        if ch == '<':
            x -= 1
        if ch == '>':
            x += 1

        presents[(x, y)] += 1

    count = 0
    for x, y in presents:
        if presents[(x, y)] > 0:
            count += 1

    print(count)

if __name__ == "__main__":
    main()

# vim: set et sw=4 ts=4:

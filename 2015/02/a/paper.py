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

def computepaper(l, w, h):
    try:
        smallside = l * w * h / max(l, w, h)
    except:
        print(l, w, h)
        raise

    return (2 * l * w + 2 * w * h + 2 * h * l + smallside)

def computeribbon(l, w, h):
    sides = sorted([l, w, h])
    return sides[0] * 2 + sides[1] * 2 + sides[0] * sides[1] * sides[2]

def main():
    with open("input.txt", "r") as f:
        lines = f.readlines()

    totalpaper = 0
    totalribbon = 0
    for line in lines:
        try:
            dimensions = line.split("x")
            l = int(dimensions[0])
            w = int(dimensions[1])
            h = int(dimensions[2])
        except:
            print("done")
            break

        totalpaper += computepaper(l, w, h)
        totalribbon += computeribbon(l, w, h)

    print(totalpaper, totalribbon)




if __name__ == "__main__":
    main()

# vim: set et sw=4 ts=4:

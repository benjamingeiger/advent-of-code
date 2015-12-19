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

from itertools import tee, izip

def pairwise(iterable):
    "s -> (s0,s1), (s1,s2), (s2, s3), ..."
    a, b = tee(iterable)
    next(b, None)
    return izip(a, b)

def isnice(s):
    print(s)
    vowels = 0
    if s[-1] in ('a', 'e', 'i', 'o', 'u'):
        vowels += 1

    hasdouble = False

    for i in range(len(s) - 1):
        if s[i] in ('a', 'e', 'i', 'o', 'u'):
            vowels += 1

        pair = s[i:i+2]
        if pair in ("ab", "cd", "pq", "xy"):
            return False
        if pair[0] == pair[1]:
            hasdouble = True

    return hasdouble and vowels >= 3

def isniceb(s):
    # pairs that don't overlap
    pairs = list(pairwise(s))
    for i in range(len(pairs)):
        if pairs[i] in pairs[i+2:]:
            break
    else:
        return False

    # repeated with one in between
    for i in range(len(s) - 2):
        if s[i] == s[i + 2]:
            return True
    else:
        return False

def main():
    with open("input.txt", "r") as f:
        lines = f.readlines()

    print(sum([1 for line in lines if isniceb(line)]))


if __name__ == "__main__":
    main()

# vim: set et sw=4 ts=4:

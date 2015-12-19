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

def substitute(s, f, t):
    results = []

    #print(s)
    chunks = s.split(f)
    #print(chunks)
    #f = "(" + f + ")"
    for i in range(1, len(chunks) ):
        #print("start", chunks[:i])
        #print("end", chunks[i:])
        results.append(f.join(chunks[:i]) + t + f.join(chunks[i:]))

    #print(f, t, results)
    return results


def main():
    with open("input.txt", "r") as f:
        lines = f.readlines()

    with open("target.txt", "r") as f:
        target = f.read().strip()

    substitutions = []
    for line in lines:
        tokens = line.split()
        substitutions.append((tokens[0], tokens[2]))

    results = set()
    for f, t in substitutions:
        results.update(substitute(target, f, t))

    print(results)
    print(len(results))



if __name__ == "__main__":
    main()

# vim: set et sw=4 ts=4:

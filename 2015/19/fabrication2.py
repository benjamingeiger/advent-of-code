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

    chunks = s.split(f)
    for i in range(1, len(chunks) ):
        results.append(f.join(chunks[:i]) + t + f.join(chunks[i:]))

    return results

possibilities = {}

def main():
    with open("input.txt", "r") as f:
        lines = f.readlines()

    with open("target.txt", "r") as f:
        target = f.read().strip()

    substitutions = []
    for line in lines:
        tokens = line.split()
        substitutions.append((tokens[2], tokens[0])) #intentionally backward

    #results = set()
    #for f, t in substitutions:
        #results.update(substitute(target, f, t))

    possibilities[target] = (0, None)

    queue = [target]

    while True:
        results = set()
        #queue.sort(key=len, reverse=True)
        cur = queue.pop()
        print(cur)
        #if len(cur) > len(target):
            #continue
        steps = possibilities[cur][0] + 1
        for f, t in substitutions:
            results.update(substitute(cur, f, t))
        if "e" in results:
            print(steps)
            break
        for v in results:
            if v not in possibilities:
                possibilities[v] = (steps, cur)
                queue.append(v)

if __name__ == "__main__":
    main()

# vim: set et sw=4 ts=4:

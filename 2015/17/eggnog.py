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

from itertools import chain, combinations

def powerset(iterable):
    "powerset([1,2,3]) --> () (1,) (2,) (3,) (1,2) (1,3) (2,3) (1,2,3)"
    s = list(iterable)
    return chain.from_iterable(combinations(s, r) for r in range(len(s)+1))

def main():
    with open("input.txt", "r") as f:
        lines = f.readlines()

    containers = sorted([int(x) for x in lines], reverse=True)
    containersets = powerset(containers)
    fittingsets = [s for s in containersets if sum(s) == 150]
    #print(fittingsets)
    #print(len(fittingsets))
    fittingsetlength = min([len(s) for s in fittingsets])
    print(sum([1 for s in fittingsets if len(s) == fittingsetlength]))




if __name__ == "__main__":
    main()

# vim: set et sw=4 ts=4:

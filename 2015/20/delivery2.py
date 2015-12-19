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

def factors(n):
    #print(list(x for x in reduce(list.__add__, 
                #([i, n//i] for i in range(1, int(n**0.5) + 1) if n % i == 0)) if x * 50 > n))
    return sum(x for x in reduce(list.__add__, 
                ([i, n//i] for i in range(1, int(n**0.5) + 1) if n % i == 0)) if x * 50 > n)

def main():
    target = 36000000 // 11

    for i in range(1, target):
    #for i in range(1, 20):
        #print(i, factors(i))
        if factors(i) >= target:
            print("found:", i)
            break
        if i % 10000 == 0:
            print(i)



if __name__ == "__main__":
    main()

# vim: set et sw=4 ts=4:

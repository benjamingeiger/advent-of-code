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

ord_a = ord('a')
idx_i = ord('i') - ord_a
idx_o = ord('o') - ord_a
idx_l = ord('l') - ord_a

def display(nums):
    print("".join((chr(i + ord_a) for i in nums)))

def parse(chrs):
    return [ord(c) - ord_a for c in chrs]

def increment(nums):
    idx = -1
    while idx > (-1 * len(nums)):
        nums[idx] += 1
        if nums[idx] > 25:
            nums[idx] = 0
            idx -= 1
        else:
            break

    return nums

def test(nums):
    # forbidden chars
    for i in nums:
        if i in (idx_i, idx_o, idx_l):
            return False

    # straight
    for i in range(len(nums) - 2):
        if nums[i + 2] - nums[i] == 2 and nums[i + 1] - nums[i] == 1:
            break
    else:
        return False

    # pairs
    pairs = list(pairwise(nums))
    doubles = [i for i, x in enumerate(pairs) if x[0] == x[1]]
    return (len(doubles) >= 2 and doubles[-1] - doubles[0] >= 2)


def main():
    puzzlekey = parse("hepxcrrq")
    #print(puzzlekey)
    #display(puzzlekey)

    tmp = increment(puzzlekey)
    while not test(tmp):
        tmp = increment(tmp)
    
    display(tmp)

    tmp = increment(tmp)
    while not test(tmp):
        tmp = increment(tmp)
    
    display(tmp)
    



if __name__ == "__main__":
    main()

# vim: set et sw=4 ts=4:

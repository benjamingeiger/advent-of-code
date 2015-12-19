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

def step(s):
    output = []
    cur = s[0]
    count = 0
    for ch in s:
        if ch == cur:
            count += 1
        else:
            output.append(str(count) + cur)
            cur = ch
            count = 1
    output.append(str(count) + cur) # get the last char

    return "".join(output)


def main():
    inval = "1113222113"
    for i in range(50):
        inval = step(inval)
        #print(inval)
        print(i)
    print(len(inval))

if __name__ == "__main__":
    main()

# vim: set et sw=4 ts=4:

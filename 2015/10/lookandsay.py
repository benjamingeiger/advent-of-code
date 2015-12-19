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
    def first(s):
        i = 0
        while i < len(s) and s[i] == s[0]:
            i += 1
        return i, s[0], s[i:]
    
    output = ""
    while len(s) > 0:
        num, val, s = first(s)
        output += str(num)
        output += str(val)

    return output

def main():
    inval = "1113222113"
    for i in range(50):
        inval = step(inval)
        print(inval)
    print(len(inval))

if __name__ == "__main__":
    main()

# vim: set et sw=4 ts=4:

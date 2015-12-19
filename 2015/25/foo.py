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


def main():
    targetrow = 2978
    targetcol = 3083
    targetleftrow = 6060

    start = 20151125
    mult = 252533
    quot = 33554393

    idx = sum(i for i in range(1, targetleftrow)) + targetcol - 1

    tmp = (pow(mult, idx, quot) * start) % quot
    print(tmp)

if __name__ == "__main__":
    main()

# vim: set et sw=4 ts=4:

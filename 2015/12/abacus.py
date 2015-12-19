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

import json

total = 0
def parse_int(s):
    global total

    tmp = int(s)
    total += tmp
    return tmp

def parse_float(s):
    global total

    tmp = float(s)
    total += float
    return tmp

def main():
    jsonfile = json.load(open("input.txt", "r"), parse_int=parse_int, parse_float=parse_float)

    print(total)


if __name__ == "__main__":
    main()

# vim: set et sw=4 ts=4:

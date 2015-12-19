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
    data = sys.stdin.read(10000)

    up = sum([1 for ch in data if ch == '('])
    down = sum([1 for ch in data if ch == ')'])

    print(up - down)

if __name__ == "__main__":
    main()

# vim: set et sw=4 ts=4:

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

import hashlib

def main():
    puzzlekey = 'iwrupvqb'

    parent = hashlib.md5()
    parent.update(puzzlekey)

    counter = 1
    while True:
        hasher = parent.copy()
        hasher.update(str(counter))
        print(counter, hasher.hexdigest())
        if hasher.hexdigest()[:6] == "000000":
            print(counter)
            break

        counter += 1

if __name__ == "__main__":
    main()

# vim: set et sw=4 ts=4:

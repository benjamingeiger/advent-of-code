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

def expand(s):
    return '"' + s.replace("\\", "\\\\").replace('"', '\\"') + '"'

def main():
    with open("input.txt", "r") as f:
        lines = f.readlines()

    enclen = 0
    ttllen = 0

    for line in lines:
        enclen += len(line.strip())
        #ttllen += len(eval(line))
        ttllen += len(expand(line.strip()))
        print(line.strip())
        print(expand(line.strip()))

    print(enclen - ttllen)

if __name__ == "__main__":
    main()

# vim: set et sw=4 ts=4:

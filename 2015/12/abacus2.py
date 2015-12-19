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

def parse_list(l):
    total = 0

    for x in l:
        if type(x) == dict:
            total += parse_dict(x)
        elif type(x) == list:
            total += parse_list(x)
        elif type(x) == int:
            total += x
        else:
            print("unknown type of item", x)

    return total

def parse_dict(d):
    total = 0

    if "red" in d.values():
        return 0

    for prop in d:
        if type(d[prop]) == dict:
            total += parse_dict(d[prop])
        elif type(d[prop]) == list:
            total += parse_list(d[prop])
        elif type(d[prop]) == int:
            total += d[prop]
        else:
            print("unknown type of item", d[prop])

    return total



def main():
    jsonfile = json.load(open("input.txt", "r"))

    print(parse_dict(jsonfile))


if __name__ == "__main__":
    main()

# vim: set et sw=4 ts=4:

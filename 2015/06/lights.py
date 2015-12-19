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

import re

"""
turn on 0,0 through 999,999 # all on, 1000000
turn off 0,50 through 9,99  # turn off 500 of them, 999500
toggle 0,0 through 999,0    # toggle the first line, so 999500 - 1000 + 50 = 998550
"""

lights = {}
for x in range(1000):
    for y in range(1000):
        lights[(x, y)] = False

def turnon(lx, ly, hx, hy):
    global lights
    for x in range(lx, hx + 1):
        for y in range(ly, hy + 1):
            lights[(x, y)] = True

def turnoff(lx, ly, hx, hy):
    global lights
    for x in range(lx, hx + 1):
        for y in range(ly, hy + 1):
            lights[(x, y)] = False
            
def toggle(lx, ly, hx, hy):
    global lights
    for x in range(lx, hx + 1):
        for y in range(ly, hy + 1):
            lights[(x, y)] = not lights[(x, y)]

instructionpattern = r"^(?P<instr>\D+)\s+(?P<lx>\d+),(?P<ly>\d+) through (?P<hx>\d+),(?P<hy>\d+)"
instructionre = re.compile(instructionpattern)

def main():
    global lights

    with open("input.txt", "r") as f:
        lines = f.readlines()

    for line in lines:
        m = re.match(instructionre, line)
        instr, lx, ly, hx, hy = m.groups()
        lx = int(lx)
        ly = int(ly)
        hx = int(hx)
        hy = int(hy)
        if lx > hx or ly > hy:
            print("AOOOGAH")
        if instr == 'turn on':
            turnon(lx, ly, hx, hy)
        elif instr == 'turn off':
            turnoff(lx, ly, hx, hy)
        elif instr == 'toggle':
            toggle(lx, ly, hx, hy)
        else:
            print("Misunderstood", instr)

    print(sum([1 for idx in lights if lights[idx]]))



if __name__ == "__main__":
    main()

# vim: set et sw=4 ts=4:

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


ingredients = {}

def verify(cookie):
    volume = sum(cookie[ing] for ing in ingredients)
    calories = sum(cookie[ing] * ingredients[ing][4] for ing in ingredients)
    return (volume == 100 and calories == 500)

def score(cookie):
    capacity   = max(sum(cookie[ing] * ingredients[ing][0] for ing in ingredients), 0)
    durability = max(sum(cookie[ing] * ingredients[ing][1] for ing in ingredients), 0)
    flavor     = max(sum(cookie[ing] * ingredients[ing][2] for ing in ingredients), 0)
    texture    = max(sum(cookie[ing] * ingredients[ing][3] for ing in ingredients), 0)

    return capacity * durability * flavor * texture



def main():
    global ingredients

    with open("input.txt", "r") as f:
        lines = f.readlines()

    for line in lines:
        tokens = line.split()
        name = tokens[0][:-1]
        capacity = int(tokens[2][:-1])
        durability = int(tokens[4][:-1])
        flavor = int(tokens[6][:-1])
        texture = int(tokens[8][:-1])
        calories = int(tokens[10])

        ingredients[name] = (capacity, durability, flavor, texture, calories)

    best = 0

    for fr in range(1, 100 + 1):
        for ca in range(1, 100 + 1):
            if ca + fr > 100:
                break

            for bu in range(1, 100 + 1):
                if ca + fr + bu > 100:
                    break

                su = 100 - (fr + ca + bu)
                recipe = {"Frosting": fr, "Candy": ca, "Butterscotch": bu, "Sugar": su}
                if not verify(recipe):
                    continue

                sc = score(recipe)
                if sc > best:
                    print("Best score:", sc)
                    best = sc

    print(best)


if __name__ == "__main__":
    main()

# vim: set et sw=4 ts=4:

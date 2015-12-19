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

WEAPON = 1
ARMOR = 2
RING = 4

allweapons = [( 8, 4, 0, "Dagger"),
              (10, 5, 0, "Shortsword"),
              (25, 6, 0, "Warhammer"),
              (40, 7, 0, "Longsword"),
              (74, 8, 0, "Greataxe")]

allarmor = [(  0, 0, 0, "No armor"),
            ( 13, 0, 1, "Leather"),
            ( 31, 0, 2, "Chainmail"),
            ( 53, 0, 3, "Splintmail"),
            ( 75, 0, 4, "Bandedmail"),
            (102, 0, 5, "Platemail")]

allrings = [(  0, 0, 0, "No ring"),
            ( 25, 1, 0, "Damage +1"),
            ( 50, 2, 0, "Damage +2"),
            (100, 3, 0, "Damage +3"),
            ( 20, 0, 1, "Defense +1"),
            ( 40, 0, 2, "Defense +2"),
            ( 80, 0, 3, "Defense +3")]

def battle(attack, armor):
    bosshp     = 109
    bossattack =   8
    bossarmor  =   2

    yourhp     = 100

    while True:
        bosshp -= max(1, attack - bossarmor)
        if bosshp <= 0:
            return True
        yourhp -= max(1, bossattack - armor)
        if yourhp <= 0:
            return False

def main():
    minsuccessful = 1000000

    for weapon in allweapons:
        for armor in allarmor:
            for leftring in allrings:
                for rightring in allrings:
                    print(weapon)
                    print(armor)
                    print(leftring)
                    print(rightring)
                    cost      = weapon[0] + armor[0] + leftring[0] + rightring[0]
                    attack    = weapon[1] + armor[1] + leftring[1] + rightring[1]
                    armorval  = weapon[2] + armor[2] + leftring[2] + rightring[2]

                    if cost >= minsuccessful:
                        continue

                    if battle(attack, armorval):
                        minsuccessful = cost

    print(minsuccessful)

if __name__ == "__main__":
    main()

# vim: set et sw=4 ts=4:

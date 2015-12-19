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

def zeros(rows,cols):
    row = []
    data = []
    for i in range(cols):
        row.append(0)
    for i in range(rows):
        data.append(row[:])
    return data

# w = list of item weight or cost
# c = the cost matrix created by the dynamic programming solution
def getUsedItems(w,c):
    # item count
    i = len(c)-1
    currentW =  len(c[0])-1
    # set everything to not marked
    marked = []
    for i in range(i+1):
        marked.append(0)            
    while (i >= 0 and currentW >=0):
        if (i==0 and c[i][currentW] >0 )or c[i][currentW] != c[i-1][currentW]:
            marked[i] =1
            currentW = currentW-w[i]
        i = i-1
    return marked

# v = list of item values or profit
# w = list of item weight or cost
# W = max weight or max cost for the knapsack
def zeroOneKnapsack(v, W):
    # c is the cost matrix
    c = []
    n = len(v)
    c = zeros(n,W+1)
    for i in range(0,n):
        #for ever possible veight
        for j in range(0,W+1):      
                    #can ve add this item to this?
            if (v[i] > j):
                c[i][j] = c[i-1][j]
            else:
                c[i][j] = max(c[i-1][j],v[i] +c[i-1][j-v[i]])
    return [c[n-1][W], getUsedItems(v,c)]

import itertools

def product(x):
    return reduce(lambda z, y: z * y, x, 1)

def main():
    with open("input.txt", "r") as f:
        lines = f.readlines()

    packages = []
    for line in lines:
        packages.append(int(line))
    #packages.reverse()

    total = sum(x for x in packages) // 4
    print(total)
    target = sum(packages) // 4

    #solution = zeroOneKnapsack(packages, sum(packages)//3)
    #combo = []
    #for i, x in enumerate(solution[1]):
        #if x > 0:
            #combo.append(packages[i])
    #print(solution)
    #print(reduce(lambda x, y: x * y, combo, 1))

    for l in range(1, len(packages)):
        combos = []
        for n in itertools.combinations(packages, l):
            if sum(n) == target:
                combos.append(n)

        print(len(combos), l)
        if len(combos) > 0:
            break

    c = min(combos, key=product)
    print(c)
    print(product(c))
        



if __name__ == "__main__":
    main()

# vim: set et sw=4 ts=4:

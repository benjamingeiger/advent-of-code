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

import networkx as nx

def hamiltonian_paths(g):
    paths = []
    pathmax = len(g.nodes())

    def propagate(g, path, pathlen):
        if len(path) == pathmax:
            paths.append((pathlen, path))
            return

        for n in g[path[-1]]:
            if n not in path:
                #print(g[path[-1]][n])
                propagate(g, path + [n], pathlen + g[path[-1]][n]['weight'])

    for start in g.nodes():
        propagate(g, [start], 0)

    return sorted(paths)

def main():
    with open("input.txt", "r") as f:
        lines = f.readlines()
    
    g = nx.Graph()

    for line in lines:
        tokens = line.split()
        print(tokens)
        src = tokens[0]
        dst = tokens[2]
        dist = int(tokens[4])

        g.add_edge(src, dst, weight=dist)

    print(hamiltonian_paths(g)[0])

if __name__ == "__main__":
    main()

# vim: set et sw=4 ts=4:

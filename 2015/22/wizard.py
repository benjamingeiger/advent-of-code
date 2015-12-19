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

class State (object):
    def __init__(self):
        self.hp        = 50
        #self.hp        = 10
        self.defense   = 0
        self.bosshp    = 55
        #self.bosshp    = 14

        
        self.effects   = []

        self.mana      = 500
        #self.mana      = 250
        self.manaspent = 0

        self.prev = None
        self.action = "initial"

    def duplicate(self):
        other = State()
        
        other.hp        = self.hp
        other.defense   = 0 # effects will rebuild it
        other.bosshp    = self.bosshp
        
        other.effects   = self.effects[:]

        other.mana      = self.mana
        other.manaspent = self.manaspent

        other.prev = self

        other.action = "copy"

        return other

    def doeffects(self):
        neweffects = []
        for time, effect, name in self.effects:
            if time > 1:
                neweffects.append((time - 1, effect, name))
            effect(self)
        self.effects = neweffects

    def bossattack(self):
        other           = self.duplicate()
        other.action    = "boss attack"
        other.doeffects()
        if other.bosshp <= 0:
            return other

        other.hp -= max((8 - other.defense), 1)

        return other

    def magicmissile(self):
        other           = self.duplicate()
        other.action    = "magic missile"
        other.hp -= 1
        if other.hp <= 0:
            return None
        other.doeffects()
        if other.bosshp <= 0:
            return other

        other.bosshp    -= 4

        other.mana      -= 53
        other.manaspent += 53
        
        if other.mana < 0:
            return None
        return other

    def drain(self):
        other           = self.duplicate()
        other.action    = "drain"
        other.hp -= 1
        if other.hp <= 0:
            return None
        other.doeffects()
        if other.bosshp <= 0:
            return other

        other.hp        += 2
        other.bosshp    -= 2

        other.mana      -= 73
        other.manaspent += 73

        if other.mana < 0:
            return None
        return other

    def shield(self):
        def effect(self):
            self.defense += 7

        other = self.duplicate()
        other.action    = "shield"
        other.hp -= 1
        if other.hp <= 0:
            return None
        other.doeffects()
        if other.bosshp <= 0:
            return other

        if "shield" in [eff for (_, _, eff) in other.effects]:
            return None

        other.effects.append((6, effect, "shield"))

        other.mana -= 113
        other.manaspent += 113

        if other.mana < 0:
            return None
        return other

    def poison(self):
        def effect(self):
            self.bosshp -= 3

        other = self.duplicate()
        other.action    = "poison"
        other.hp -= 1
        if other.hp <= 0:
            return None
        other.doeffects()
        if other.bosshp <= 0:
            return other

        if "poison" in [eff for (_, _, eff) in other.effects]:
            return None
        
        other.effects.append((6, effect, "poison"))

        other.mana -= 173
        other.manaspent += 173

        if other.mana < 0:
            return None
        return other

    def recharge(self):
        def effect(self):
            self.mana += 101

        other = self.duplicate()
        other.action    = "recharge"
        other.hp -= 1
        if other.hp <= 0:
            return None
        other.doeffects()
        if other.bosshp <= 0:
            return other

        if "recharge" in [eff for (_, _, eff) in other.effects]:
            return None

        other.effects.append((5, effect, "recharge"))

        other.mana -= 229
        other.manaspent += 229

        if other.mana < 0:
            return None
        return other


def main():
    minsuccessful = State()
    minsuccessful.manaspent = 1000000

    initstate = State()

    states = [initstate]

    runeffects = [lambda s: s.magicmissile(), lambda s: s.drain(), lambda s: s.shield(), lambda s: s.poison(), lambda s: s.recharge()]

    while states:
        newstates = []
        #print(len(states))
        print(min(s.manaspent for s in states), max(s.manaspent for s in states))
        for state in states:
            for eff in runeffects:
                tmp = eff(state)
                if tmp is not None:
                    if tmp.bosshp <= 0:
                        if tmp.manaspent < minsuccessful.manaspent:
                            minsuccessful = tmp
                            print("update:", tmp.manaspent)
                    elif tmp.manaspent > minsuccessful.manaspent:
                        continue
                    else:
                        newstates.append(tmp)
        states = newstates
        newstates = []
        for state in states:
            tmp = state.bossattack()
            if tmp is not None:
                if tmp.bosshp <= 0:
                    if tmp.manaspent < minsuccessful.manaspent:
                        minsuccessful = tmp
                        print("update:", tmp.manaspent)
                        continue
                if tmp.hp <= 0:
                    continue # you lose
                newstates.append(tmp)
        states = newstates

    

    print("answer:", minsuccessful)

    tmp = minsuccessful
    states = []
    while tmp is not None:
        #print("{:15}\t{}\t{}\t{}\t{}".format(tmp.action, tmp.mana, tmp.manaspent, tmp.bosshp, tmp.hp))
        #print([(x, z) for x, y, z in tmp.effects])
        states.append(tmp)
        tmp = tmp.prev
    states.reverse()
    for tmp in states:
        print("{:15}\t{}\t{}\t{}\t{}".format(tmp.action, tmp.mana, tmp.manaspent, tmp.bosshp, tmp.hp))
        print([(x, z) for x, y, z in tmp.effects])

def test2():
    tmp = State()
    tmp = tmp.poison()
    tmp = tmp.poison()
    print(tmp.effects)

def test():
    tmp = State()
    print("poison")
    tmp = tmp.poison()
    print(tmp.bosshp)
    print("boss attack")
    tmp = tmp.bossattack()
    print(tmp.bosshp)
    print("magic missile")
    tmp = tmp.magicmissile()
    print(tmp.bosshp)
    print("boss attack")
    tmp = tmp.bossattack()
    print(tmp.bosshp)
    print("magic missile")
    tmp = tmp.magicmissile()
    print(tmp.bosshp)
    print("boss attack")
    tmp = tmp.bossattack()
    print(tmp.bosshp)
    print("magic missile")
    tmp = tmp.magicmissile()
    print(tmp.bosshp)
    print("boss attack")
    tmp = tmp.bossattack()
    print(tmp.bosshp)

if __name__ == "__main__":
    main()
    #test2()
    #test()

# vim: set et sw=4 ts=4:

deckcount = 119315717514047
shufflecount = 101741582076661

instructionstext = open("input.txt", "r").readlines()


offset_diff = 0
increment_mul = 1
for inst in instructionstext:
    print(inst)
    if inst.strip() == "deal into new stack":
        increment_mul *= -1
        offset_diff += increment_mul
        offset_diff %= deckcount
    elif inst[:20] == "deal with increment ":
        n = int(inst[20:])
        increment_mul *= pow(n, deckcount - 2, deckcount)
        increment_mul %= deckcount
    elif inst[:4] == "cut ":
        n = int(inst[4:])
        offset_diff += increment_mul * n
        offset_diff %= deckcount

    print(offset_diff, increment_mul)

finalincrement = pow(increment_mul, shufflecount, deckcount)

finaloffset = offset_diff * (1 - finalincrement) * pow(1 - increment_mul, deckcount - 2, deckcount)
finaloffset %= deckcount

# print(finalincrement, finaloffset)
print(finaloffset, finalincrement)

pos2020 = (finaloffset + finalincrement * 2020) % deckcount

print(pos2020)

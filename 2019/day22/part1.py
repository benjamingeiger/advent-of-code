deckcount = 10007
# deckcount = 10
deck = list(range(deckcount))

instructions = open("input.txt", "r").readlines()
# instructions = ["deal with increment 3"]
# instructions = ["cut -4"]
# instructions = ["deal into new stack"]

for inst in instructions:
    if inst.strip() == "deal into new stack":
        print("deal")
        deck = list(reversed(deck))
    elif inst[:20] == "deal with increment ":
        pos = int(inst[20:])
        print(f"deal {pos}")
        newdeck = deck[:]
        for i, card in enumerate(deck):
            newdeck[((i * pos) % deckcount)] = card
        deck = newdeck
    elif inst[:4] == "cut ":
        pos = int(inst[4:])
        print(f"cut {pos}")
        deck = deck[pos:] + deck[:pos]

print(deck)

print(deck.index(2019))

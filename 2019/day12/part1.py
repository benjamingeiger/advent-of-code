initialpositions = [(3, 15, 8, 0, 0, 0), (5, -1, -2, 0, 0, 0), (-10, 8, 2, 0, 0, 0), (8, 4, -5, 0, 0, 0)]

# initialpositions = [(-1,0,2,0,0,0), (2,-10,-7,0,0,0), (4,-8,8,0,0,0), (3,5,-1,0,0,0)]

bodies = initialpositions[:]

e = 0
for iteration in range(1000):
    print("iteration", iteration + 1)
    newbodies = []
    e = 0
    for x, y, z, dx, dy, dz in bodies:
        # do gravity
        for (xp, yp, zp, _, _, _) in bodies:
            if xp > x: dx += 1
            elif xp < x: dx -= 1
            if yp > y: dy += 1
            elif yp < y: dy -= 1
            if zp > z: dz += 1
            elif zp < z: dz -= 1

        newbodies.append((x + dx, y + dy, z + dz, dx, dy, dz))
        print(newbodies[-1], end=" ")

        # compute the new energy
        pe = abs(x + dx) + abs(y + dy) + abs(z + dz)
        ke = abs(dx) + abs(dy) + abs(dz)
        e += pe * ke

        print(pe, ke, pe * ke, e)

    bodies = newbodies

print(e)



from collections import defaultdict

def process(wire):
    space = defaultdict(int)
    
    x = 0
    y = 0
    dist = 0

    segments = wire.split(",")
    for segment in segments:
        direction = segment[0]
        distance = int(segment[1:])

        for _ in range(distance):
            space[(x, y)] = dist
            dist += 1
            if direction == "L":
                x -= 1
            elif direction == "R":
                x += 1
            elif direction == "U":
                y -= 1
            elif direction == "D":
                y += 1
            else:
                print("invalid direction")

    return space


with open("input.txt", "r") as f:
    wire1 = process(f.readline())
    wire2 = process(f.readline())

    print(wire1)
    print(wire2)

    intersections = set(wire1.keys()).intersection(set(wire2.keys()))

    print(intersections)
    intersections.remove((0, 0))

    nearest = None
    nearestdistance = 999999999999
    for intersection in intersections:
        if wire1[intersection] + wire2[intersection] < nearestdistance:
            nearest = intersection
            nearestdistance = wire1[intersection] + wire2[intersection]

    print(nearest)
    print(nearestdistance)



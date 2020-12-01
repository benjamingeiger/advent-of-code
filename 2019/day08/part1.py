f = open("input.txt", "r")
image = list(f.readline()[:-1])

print(len(image))

width = 25
height = 6

print(len(image) / (width * height))

def chunks(lst, n):
    """Yield successive n-sized chunks from lst."""
    for i in range(0, len(lst), n):
        yield lst[i:i + n]

layers = list(chunks(image, (width * height)))

num_0 = width * height
result = 0
for layer in layers:
    foo = sum(1 for x in layer if x == '0')
    print(foo)
    if foo < num_0:
        num_0 = foo
        n_1 = sum(1 for x in layer if x == '1')
        n_2 = sum(1 for x in layer if x == '2')
        result = n_1 * n_2

print(result)

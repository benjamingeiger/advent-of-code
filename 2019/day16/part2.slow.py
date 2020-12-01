from itertools import cycle

digits = [int(x) for x in open("input.txt", "r").readline().strip()]
# digits = [0,3,0,3,6,7,3,2,5,7,7,2,1,2,9,4,4,0,6,3,4,9,1,5,6,5,4,7,4,6,6,4]

digits *= 10000

offset = int("".join(str(d) for d in digits[:7]))

digits = digits[offset:]

def fftphase(digits):
    outdigits = []
    for i in range(len(digits)):
        if i % 1000 == 0:
            print("    digit", i)
        vs = digits
        outdigits.append(abs(sum(vs[i:])) % 10)
    # print(outdigits)
    return outdigits

for i in range(100):
    print("phase", i)
    digits = fftphase(digits)

print("".join(str(d) for d in digits[:8]))

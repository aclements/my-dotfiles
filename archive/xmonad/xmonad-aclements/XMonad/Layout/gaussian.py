import sys
from math import *

sigma = 2.0
bmax = int(ceil(3*sigma))
bmin = -bmax

def twod():
    m = [[0]*(bmax-bmin+1) for _ in range(bmin, bmax+1)]

    for y in range(bmin, bmax+1):
        for x in range(bmin, bmax+1):
            m[x-bmin][y-bmin] = 1/(2*pi*sigma**2)*exp(-(x**2+y**2)/(2*sigma**2))

    for r in m:
        print r
    print

    # Truncate small values
    maxv = max(max(r) for r in m)
    for r in m:
        for i, v in enumerate(r):
            if v < maxv / 256:
                r[i] = 0

    for r in m:
        print r
    print

    # Normalize
    total = sum(sum(r) for r in m)
    for r in m:
        for i, v in enumerate(r):
            r[i] = v/total

    for r in m:
        print r
    print

    return m

def oned():
    m = [0] * (bmax-bmin+1)

    for x in range(bmin, bmax+1):
        m[x-bmin] = 1/(sqrt(2*pi)*sigma)*exp(-x**2/(2*sigma**2))

    # Normalize
    total = sum(m)
    for i, v in enumerate(m):
        m[i] = v/total

    return m

for r in twod():
    for v in r:
        sys.stdout.write("%.4f " % v)
    sys.stdout.write("\n")
print
m = oned()
for v1 in m:
    for v2 in m:
        sys.stdout.write("%.4f " % (v1*v2))
    sys.stdout.write("\n")

print oned()

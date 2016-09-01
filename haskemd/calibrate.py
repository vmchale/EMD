import haskemd
import pyemd
import numpy as np
from numpy.random import randint
import numpy.random

def ham_dist(x, y):
    total = 0
    a1 = bin(x)[2::]
    a2 = bin(y)[2::]
    if len(a1) > len(a2):
        a2=a2.rjust(len(a1), '0')
    else:
        a1=a1.rjust(len(a2), '0')
    for i in range(max(len(a1),len(a2))):
        total=total+abs(int(a1[i])-int(a2[i]))
    return float(total)

def bettermarkov(num):
    return abs(np.cumsum(numpy.random.binomial(2,(1/num),num))).astype(np.uint32)

def bernoulli(num):
    return numpy.random.binomial(2,(1/2),num)

def better(num):
    return numpy.random.binomial(num,(1/num),num)

def skewed(num):
    return numpy.random.binomial(4*num,(1/(num*4)),num)

def equalize(sinks, sources):
    if sum(sinks) > sum(sources):
        sources = np.append(sources, sum(sinks)-sum(sources))
        sinks = np.append(sinks, 0)
    else:
        sinks = np.append(sinks, sum(sources)-sum(sinks))
        sources = np.append(sources, 0)
    return (sinks, sources)

#for x in [256,512,1024,2048,4096]:
#    distance_metric = np.fromfunction(np.vectorize(ham_dist), (x,x), dtype=int)
#    np.save("numpy/mat"+str(x)+".npy",distance_metric)

sizes = [256,256,256,256,256,256,256,256,256,256,512,512,512,512,512,512,512,512,512,512,1024,1024,1024,1024,1024,1024,1024,1024,1024,1024,2048,2048,2048,2048,2048,2048,2048,2048,2048,2048,4096,4096,4096,4096,4096,8192]

for x in sizes:
    a = equalize(bettermarkov(x-1),better(x-1))
    distance_metric=np.load("numpy/mat"+str(x)+".npy")
    data = (haskemd.testemd(a[0],a[1])) + (pyemd.emd(a[0].astype(float), a[1].astype(float), distance_metric),) + (x,)
    with open("data.dat","a") as file:
        file.write(str(data)+"\n")

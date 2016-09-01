import numpy as np
import matplotlib.pyplot as plt
import sys

with open("data.dat",'r') as data:
    parsed = [ tuple(map(float, i.strip("()\n").split(','))) for i in data ]

arr = [ np.expand_dims(np.asarray(x, dtype=np.float64), 1) for x in parsed ]

table = (np.concatenate(arr, axis=1))

filt=float(sys.argv[1])
a = np.transpose(table)
f = a# (a[ (1.3>a[:,1]) & (.9<a[:,1]) ])
filtered = (f[ (filt==f[:,4]) ])
table = np.transpose(filtered)

haskemd = table[0]
plus = table[1]
var = table[2]
pyemd=table[3]
nodes=table[4]

factor = pyemd/haskemd

#slope should be halved each time we double the bins/add a node
fit=np.polyfit(var, factor, 1)
predict = (var)*fit[0]+fit[1]
print(predict/factor)
#basically we're having a problem with low variance
#so we need to screw with the variance if we can; ideally without messing with the mean

plt.scatter(var/plus, factor)
plt.show()

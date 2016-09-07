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

fit=np.polyfit(var/(plus), factor, 1)
r=str(np.corrcoef(var/(plus), factor)[(1,0)])
with open("regression.dat",'a') as output:
    output.write(str(filt)+" "+str(fit)+" "+r+"\n")
predict = (var/(plus))*fit[0]+fit[1]
x = np.linspace(min(var/plus), max(var/plus))
line = plt.plot(x, x*fit[0]+fit[1], c='red')

plt.scatter(var/plus, factor)
plt.xlabel('Variance')
plt.ylabel('Factor')
plt.savefig('data'+str(int(filt))+'.png')

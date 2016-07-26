import numpy as np
from numpy.random import randint
import subprocess
import sys
from os.path import isfile
import os
from PIL import Image
from fractions import Fraction

path = os.environ["HASKPATH"]

def sinksrand(num):
    return randint(0,num,num,np.uint32)

def sourcesrand(num):
    return randint(0,num,num,np.uint32)

def tobmp (lists, filename):
    arr = np.uint32(np.int32(lists))
    arr = np.expand_dims(arr, axis=1)
    arr = np.expand_dims(arr, axis=1)
    arr = np.expand_dims(arr, axis=1)
    img = Image.fromarray(arr, 'RGBA')
    img.save(path+filename+'.png')
    subprocess.Popen(["mogrify","-format","bmp",filename+'.png'], cwd=path).wait()

def emd(list1, list2):
    l1 = tobmp (list1, "list1")
    l2 = tobmp (list2, "list2")
    if not isfile(path+"data/matrix-"+(str(list1.size))):
        makemat(list1)
    output = haskemd("list1.bmp","list2.bmp","data/matrix-"+(str(list1.size)))
    output = str(output)
    return float(''.join(filter(lambda x: x.isdigit(), output)))

def haskemd(list1, list2, matrix):
    p=subprocess.Popen(["sudo", "stack", "exec", "--allow-different-user", "EMD-exe",str(list1),str(list2),matrix], cwd=path, stdout=subprocess.PIPE)##.wait()
    out, trash = p.communicate()
    cleanup()
    return out

def makemat(l):
    dim = l.size
    subprocess.Popen (["stack","exec","Mat-exe",str(dim),"data/matrix-"+str(dim)], cwd=path).wait()

def cleanup():
    ##subprocess.call (["rm","matrix-*"])
    subprocess.Popen (["rm","list1.bmp","list2.bmp","list1.png","list2.png"], cwd=path).wait()

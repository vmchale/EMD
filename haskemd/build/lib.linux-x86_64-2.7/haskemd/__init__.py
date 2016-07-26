import numpy as np
from numpy.random import randint
import subprocess
import sys
from os.path import isfile
import os
from PIL import Image
from fractions import Fraction

path = os.environ["HASKPATH"]

def sinksrand():
    return randint(0,1024,1024,np.uint32)

def sourcesrand():
    return randint(0,1024,1024,np.uint32)

def tobmp (lists, filename):
    arr = np.uint32(np.int32(lists))
    arr = np.expand_dims(arr, axis=1)
    arr = np.expand_dims(arr, axis=1)
    arr = np.expand_dims(arr, axis=1)
    img = Image.fromarray(arr, 'RGBA')
    print(path)
    img.save(path+filename+'.png')
    subprocess.Popen(["mogrify","-format","bmp",filename+'.png'], cwd=path)

def emd(list1, list2):
    l1 = tobmp (list1, "list1")
    l2 = tobmp (list2, "list2")
    if not isfile(path+"data/matrix-"+(str(list1.size))):
        makemat(list1)
    return haskemd("list1.bmp","list2.bmp","data/matrix-"+(str(list1.size)))

def haskemd(list1, list2, matrix):
    subprocess.Popen(["sudo", "stack", "exec", "--allow-different-user", "EMD-exe",str(list1),str(list2),matrix], cwd=path)
    cleanup()

def makemat(l):
    dim = l.size
    subprocess.Popen (["stack","exec","Mat-exe",str(dim),"data/matrix-"+str(dim)], cwd=path)

def cleanup():
    ##subprocess.call (["rm","matrix-*"])
    subprocess.Popen (["rm","list1.bmp","list2.bmp","list1.png","list2.png"], cwd=path)

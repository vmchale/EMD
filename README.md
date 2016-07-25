# EMD
Earth mover's distance on Nvidia GPUS

This provides a python wrapper to code which can be imported as haskemd. This is the easiest way to use the python wrapper as it takes Numpy arrays.

To test:

```
python3
>>>import haskemd
>>>haskemd.emd(haskemd.sinksrand, haskemd.sourcesrand)
```

##Prereqs
To install CUDA, download it directly from NVIDIA, along with the most up-to-date driver for your platform. 

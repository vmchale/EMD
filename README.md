#EMD
Earth mover's distance on Nvidia GPUS

##Building
To build, you must install stack with
```
wget -qO- https://get.haskellstack.org/ | sh
```
Then type `stack build`

##Python
The provided python wrapper takes two numpy arrays and computes the EMD:
```
python3
>>>import haskemd
>>>haskemd.emd(haskemd.sinksrand, haskemd.sourcesrand)
```

##CUDA
CUDA and relevant NVIDIA drivers have to be installed from their site.

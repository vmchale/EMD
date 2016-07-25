#EMD
Earth mover's distance on Nvidia GPUS

##CUDA
CUDA and relevant NVIDIA drivers have to be installed from their site. This should be done first.

##Building
To build, you must install stack with
```
wget -qO- https://get.haskellstack.org/ | sh
```
Then type `stack setup` followed by `stack build`

##Python
The provided python wrapper takes two numpy arrays and computes the EMD:
```
python3
>>>import haskemd
>>>haskemd.emd(haskemd.sinksrand, haskemd.sourcesrand)
```


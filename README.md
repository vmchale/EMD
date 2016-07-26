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

Once this is done `cd` into `haskellemd` and run
```
sudo python3 setup.py install
```

##Python
The provided python wrapper takes two (one-dimensional) numpy arrays and computes the EMD:
```
python3
>>>import haskemd
>>>haskemd.emd(haskemd.sinksrand(1024), haskemd.sourcesrand(1024))
692959.0
```
`sinksrand(1024)` and `sourcesrand(1024)` just generates test data with 

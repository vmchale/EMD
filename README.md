#EMD
Earth mover's distance on Nvidia GPUs

##CUDA
CUDA and relevant NVIDIA drivers have to be installed from their site. This should be done first.

##Building
To build, you must install stack with
```
wget -qO- https://get.haskellstack.org/ | sh
```
Then type `stack setup` followed by `stack build`

Then run the script `start.sh`

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
`sinksrand(1024)` and `sourcesrand(1024)` just generates test data with 1024 bins. 

To try your own, consider the following example:
```
>>>import haskemd
>>>import numpy as np
>>>a1 = np.array([1,2,3,4])
>>>a2 = np.array([4,3,2,1])
>>>haskemd(a1, a2)
6.0
```

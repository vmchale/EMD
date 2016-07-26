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

The script `start.sh` will create common points between python and haskell. If the script does not work on your platform, download imagemagick for your platform and create an environment variable `HASKPATH` with with the path to `EMD/` e.g. `HASKPATH=/home/username/EMD/`

Once this is done `cd` into `haskellemd` and run
```
sudo python3 setup.py install
```

After this, the haskemd module can be used from any directory.

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

##Ground distance
In our case, the "ground distance" is the hamming distance. Moreover, the matrix for the metric is automatically generated. This means that an array like
```
np.array([0.25,0.5])
```
must have the correct values in the correct places. In fact, d<sub>ij</sub> is based on the indices. Here, d<sub>01</sub> would mean the distance between the bin containing 0.25 and the bin containing 0.5. In general, we have d<sub>ij</sub>=hammingDistance i j

##Hamming distance
To get the hamming distance from a integer indices, say 3 and 7, we first convert to binary to get 011 and 111. Then we can compute the hamming distance to get 1 (in this case). Hence d<sub>37</sub>=1

##Stored matrices
After the matrix for a metric is computed, it is stored in the `data/` directory. This means that if you compute an EMD between distributions with 8192 bins, every subsequent calculation between distributions with 8192 bins will be faster. As the number of bins gets larger, it becomes necessary to store these matrices in order to guarantee quick runtime. 

However, this also means that the data/directory can get pretty large: up to 257M for 8192 bins, or 1.1G for 16384 bins. 

##Computers without an NVIDIA GPU
If you want to run the code on your CPU, you can can edit the code in `src/Edmonds.hs` Open it up and the first lines should look like this:
```
{-# LANGUAGE FlexibleContexts #-}

module Edmonds
         (
         exec) where

import Data.Array.Accelerate as A
import Data.Array.Accelerate.IO
import Data.Array.Accelerate.CUDA
--import Data.Array.Accelerate.Interpreter
import System.Environment
import System.IO.Unsafe (unsafePerformIO)
```
Comment out the line with `Data.Array.Accelerate.CUDA` and uncomment the line with `Data.Array.Accelerate.Interpreter` Your file should now look like this:
```
{-# LANGUAGE FlexibleContexts #-}

module Edmonds
         (
         exec) where

import Data.Array.Accelerate as A
import Data.Array.Accelerate.IO
--import Data.Array.Accelerate.CUDA
import Data.Array.Accelerate.Interpreter
import System.Environment
import System.IO.Unsafe (unsafePerformIO)
```
Save the file and run `stack build` to build it. 

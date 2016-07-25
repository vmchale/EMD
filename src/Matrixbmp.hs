module Matrixbmp
        (
        exec) where

import ReadDist
import System.Environment
import Data.Array.Accelerate as A
import Data.Array.Accelerate.IO

exec :: IO ()
exec = do
    (dim:filename:_) <- getArgs
    let mat = Matrixbmp.generateMatrix (read dim)
    writeImageToBMP filename mat

generateMatrix :: Int -> Array DIM2 RGBA32
generateMatrix a = A.fromList sh list :: Array DIM2 Word32
    where sh = (Z:.a:.a)
          list = [ Prelude.fromIntegral (hammingDistance n m) | n <- [0..(a-1)], m <- [0..(a-1)]]

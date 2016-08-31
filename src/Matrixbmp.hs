module Matrixbmp
        (
        exec) where

import ReadDist
import System.Environment
import Data.Array.Accelerate as A
import Data.Array.Repa.IO.DevIL
import Data.Array.Repa.Repr.ForeignPtr (F)
import Data.Array.Repa as R

-- | writes a metric to file as an 8-bit PNG. Metric is distance supplied in ReadDist (in our case the Hamming Distance.
exec :: IO ()
exec = do
    (dim:filename:_) <- getArgs
    mat' <- generateMatrix' (read dim)
    runIL $ writeImage (filename Prelude.++".png") mat'

-- | generates a matrix such that a_ij = (hammingDistance i j)
generateMatrix' :: Int -> IO Image
generateMatrix' a = mat >>= (return . Grey)
    where mat = (computeP $ R.fromFunction (R.Z R.:.a R.:. a) (\(R.Z R.:.m R.:.n) -> Prelude.fromIntegral (distance m n))) :: IO (R.Array F R.DIM2 Word8)

generateMatrix'' :: Int -> IO Image
generateMatrix'' dim = mat >>= (return . Grey)
    where mat = (computeP $ R.fromFunction (R.Z R.:.dim R.:. dim) (\(R.Z R.:.m R.:.n) -> Prelude.fromIntegral (hammingDistance' m n p))) :: IO (R.Array F R.DIM2 Word8)
          p = powers nodes
          nodes = Prelude.floor (logBase 2 (Prelude.fromIntegral dim)) :: Int
--possible approach: make a 3-d repa array with all the digits?

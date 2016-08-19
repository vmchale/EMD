module Matrixbmp
        (
        exec) where

import ReadDist
import System.Environment
import Data.Array.Accelerate as A
import Data.Array.Accelerate.IO
import Data.Array.Repa.IO.DevIL
import Data.Array.Repa.Repr.ForeignPtr (F)
import Data.Array.Repa as R
import Control.Monad.Identity (Identity)

-- | writes a metric to file as a 32-bit bitmap. Metric is hamming distance.
exec :: IO ()
exec = do
    (dim:filename:_) <- getArgs
    --let mat = Matrixbmp.generateMatrix (read dim)
    mat' <- generateMatrix'' (read dim)
    runIL $ writeImage (filename Prelude.++".png") mat'
    --writeImageToBMP filename mat

-- | generates a matrix such that a_ij = (hammingDistance i j)
generateMatrix :: Int -> A.Array A.DIM2 RGBA32
generateMatrix a = A.fromList sh list :: A.Array A.DIM2 Word32
    where sh = (A.Z A.:. a A.:.a)
          list = [ Prelude.fromIntegral (hammingDistance n m) | n <- [0..(a-1)], m <- [0..(a-1)]]

generateMatrix' :: Int -> Image
generateMatrix' a = Grey (copyS $ (R.fromListUnboxed sh list :: R.Array U R.DIM2 Word8))
    where sh = (R.Z R.:. a R.:. a)
          list = [ Prelude.fromIntegral (hammingDistance n m) | n <- [0..(a-1)], m <- [0..(a-1)]]

generateMatrix'' :: Int -> IO Image
generateMatrix'' a = mat >>= (return . Grey) --mat >>= (return . Grey)
    where mat = (computeP $ R.fromFunction (R.Z R.:.a R.:. a) (\(R.Z R.:.m R.:.n) -> Prelude.fromIntegral (hammingDistance m n))) :: IO (R.Array F R.DIM2 Word8)

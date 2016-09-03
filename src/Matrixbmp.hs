{-# LANGUAGE FlexibleContexts #-}

module Matrixbmp
        (
        exec) where

import ReadDist
import System.Environment
import Data.Array.Accelerate as A
import Data.Array.Accelerate.IO
import Data.Array.Accelerate.CUDA
import Data.Array.Repa.IO.DevIL
import Data.Array.Repa.Repr.ForeignPtr (F)
import Data.Array.Repa as R

-- | writes a metric to file as an 8-bit PNG. Metric is distance supplied in ReadDist (in our case the Hamming Distance.
exec :: IO ()
exec = do
    (dim:filename:_) <- getArgs
    mat <- generateMatrix'' (read dim)
--    mat' <- makeImage (generateMatrixCUDA (read dim))
--    runIL $ writeImage (filename Prelude.++"CUDA.png") mat'
    runIL $ writeImage (filename Prelude.++".png") mat

-- | generates a matrix such that a_ij = (hammingDistance i j)
generateMatrix' :: Int -> IO Image
generateMatrix' a = mat >>= (return . Grey)
    where mat = (computeP $ R.fromFunction (R.Z R.:.a R.:. a) (\(R.Z R.:.m R.:.n) -> Prelude.fromIntegral (distance m n))) :: IO (R.Array F R.DIM2 Word8)

generateMatrix'' :: Int -> IO Image
generateMatrix'' dim = mat >>= (return . Grey)
    where mat = (computeP $ R.fromFunction (R.Z R.:.dim R.:. dim) (\(R.Z R.:.m R.:.n) -> Prelude.fromIntegral (hammingDistance' m n p))) :: IO (R.Array F R.DIM2 Word8)
          p = powers nodes
          nodes = Prelude.floor (logBase 2 (Prelude.fromIntegral dim)):: Int

makeImage :: Acc (A.Array A.DIM2 Word8) -> IO Image
makeImage mat = repamat >>= (return . Grey)
    where repamat = copyP (toRepa $ run mat) :: IO (R.Array F R.DIM2 Word8)

generateMatrixCUDA :: Int -> Acc (A.Array A.DIM2 Word8)
generateMatrixCUDA dim = fold (+) (constant 0) cube
    where cube = generate (index3 dim dim nodes) (\ix -> let (A.Z A.:. m A.:. n A.:. t) = unlift ix in (let p = extension A.! (index1 t) in ((m `mod` (2*p) >=* p)==*(n `mod` (2*p) >=* p)) ? (0,1)))
          extension = toVect (Prelude.map (2^) [0..nodes] :: [Int])
          nodes = Prelude.floor (logBase 2 (Prelude.fromIntegral dim)) :: Int

index3 i j k= lift (A.Z A.:. i A.:. j A.:. k)

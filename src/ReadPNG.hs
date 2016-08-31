module ReadPNG where

import Data.Array.Repa.IO.DevIL
import Data.Array.Repa as R
import Data.Array.Repa.Repr.ForeignPtr
import Data.Array.Accelerate.IO
import Data.Array.Accelerate as A
import Data.Array.Accelerate.Interpreter
import Control.Monad
import ReadDist
import Codec.BMP

exec = testRead >>= putStrLn

testRead :: IO String
testRead = (liftM show) $ readImage' "matrix-1024.png"

trim :: A.Array A.DIM3 Word8 -> A.Array A.DIM2 Word8
trim im = run $ A.slice (use im) (lift (A.Z A.:. A.All A.:. A.All A.:. (0::Int)))

readImage' :: FilePath -> IO (A.Array A.DIM2 Word8)
readImage' = (liftM fromRepa) . (flip (>>=) copyP) . (liftM strip) . runIL . readImage

strip :: Image -> (R.Array F R.DIM2 Word8)
strip (Grey x) = x
strip _ = error "Image corrupted. Did you use Matrixbmp to generate it?"

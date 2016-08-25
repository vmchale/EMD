import ReadDist
import System.Environment
import Data.Array.Accelerate.IO
import Data.Array.Accelerate as A

exec :: IO ()
exec = do
    (dim:filename:_) <- getArgs
    let mat1 = Main.generateMatrix1 (read dim)
    let mat2 = Main.generateMatrix2 (read dim)
    writeImageToBMP (filename Prelude.++ "f") mat1
    writeImageToBMP (filename Prelude.++ "s") mat2

generateMatrix1 :: Int -> Array DIM2 RGBA32
generateMatrix1 a = A.fromList sh list :: Array DIM2 Word32
    where sh = (Z:.a:.1)
          list = Prelude.map (Prelude.fromIntegral) (Prelude.fst (testList a))

generateMatrix2 :: Int -> Array DIM2 RGBA32
generateMatrix2 a = A.fromList sh list :: Array DIM2 Word32
    where sh = (Z:.a:.1)
          list = Prelude.map (Prelude.fromIntegral) (Prelude.snd (testList a))

{-# LANGUAGE FlexibleContexts #-}

module Edmonds
         (
         exec) where

import Data.Array.Accelerate as A
import Data.Array.Accelerate.IO
import Data.Array.Accelerate.CUDA
import System.Environment
import System.IO.Unsafe (unsafePerformIO)
import ReadDist
import Codec.BMP
import Control.Monad

type Weight = Acc (Scalar Int)
type Index = Acc (Scalar Int)
type IndexList = Acc (Array DIM1 InIndex, Array DIM1 InIndex)
type IndexWeightList = Acc (Array DIM1 ((InIndex, InIndex), InWeight))
type InWeight = Int
type InIndex = Int
data Graph = Graph { distances :: Acc (Array DIM2 Word32)
                   , nodes :: Acc (Array DIM1 InWeight)
                   }

type Pseudoflow = Acc (Array DIM2 InWeight)
type Potential = Acc (Array DIM1 InWeight)

exec :: IO()
exec = do
    (list1:list2:metricname:_) <- getArgs
    let reduced = (liftM2 (liftM2 red)) (readImageFromBMP list1) (readImageFromBMP list2)
    let metric = readImageFromBMP metricname
    (let tl = (liftM2 (liftM2 takeLists')) in tl reduced metric >>= (return . show)) >>= putStrLn
    putStrLn "finished."

thevect :: (Elt a) => Acc (Array DIM2 a) -> Acc (Array DIM1 a)
thevect mat = slice mat (lift (Z:.All:.(0::Int)))

red :: Array DIM2 Word32 -> Array DIM2 Word32 -> Array DIM1 Int
red a b = run $ (A.zipWith (-) (A.map A.fromIntegral (thevect $ use a)) (A.map A.fromIntegral (thevect $ use b)))

takeLists' :: Array DIM1 Int -> Array DIM2 Word32 -> String
takeLists' reduced metric = show $ run $ calcEMD' graph x delta extra powers
    where x = empty
          extra = lift (e, e) :: IndexList
          e = toVect []
          delta = unit $ constant $ 2^(Prelude.ceiling (logBase 2 (u)) :: Int)
          powers = toVect (Prelude.map (2^) [1..iter]) :: Acc (Vector Int)
          iter = (Prelude.ceiling (logBase 2 (u)) :: Int)
          u = read $ show (Prelude.maximum (Prelude.map abs (toList reduced))) :: Float
          graph = Graph {distances=(use metric), nodes=(use reduced)}
          empty = fill (shape (use metric)) (constant 0 :: Exp Int)

calcEMD' :: Graph -> Pseudoflow -> Weight -> IndexList -> Acc (Vector Int) -> Weight
calcEMD' graph x delta extra powers = if
    getBool (the delta >=* 1) then
    (A.zipWith (+) (augmentTotal graph x delta extra powers) (calcEMD' graph x (A.map (`div` 2) delta) (nodesLeftOver graph x delta extra) powers)) else
    (unit 0)

indices :: Acc (Array DIM1 Int) -> Acc (Array DIM1 InIndex)
indices arr = enumFromN (shape (arr)) (0)

sSet :: Graph -> Pseudoflow -> Weight -> Acc (Array DIM1 InIndex)
sSet graph x delta = A.filter (\i -> i >=* 0) zipped
    where zipped = A.zipWith (\a b -> (a `mod` (2 * the delta) >=* the delta &&* a >=* 0) ? (b, constant 0-1)) (nodes graph) (indices $ nodes graph)

tSet :: Graph -> Pseudoflow -> Weight -> Acc (Array DIM1 InIndex)
tSet graph x delta = A.filter (\i -> i >=* 0) zipped
    where zipped = A.zipWith (\a b -> ((-a) `mod` (2 * the delta) >=* (the delta) &&* a <=* 0) ? (b, constant 0-1)) (nodes graph) (indices $ nodes graph)

imbalances :: Graph -> Pseudoflow -> Acc (Array DIM1 InWeight)
imbalances graph x = A.zipWith (+) (nodes graph) (A.fold (+) 0 flows)
    where flows = (A.zipWith (-) (transpose x) x)

getBool :: Exp Bool -> Bool
getBool b = (toList $ run $ unit $ b) == [True]

augmentTotal :: Graph -> Pseudoflow -> Weight -> IndexList -> Acc (Vector Int) -> Weight
augmentTotal graph x delta extra powers = A.zipWith (*) delta (A.sum totalDist)
    where totalDist = A.map (\a -> let (k,v) = unlift a in A.fromIntegral $ (distances graph) A.! (index2 k v)) imbalanced
--    where totalDist = A.map (\a -> let (k,v) = unlift a in (the $ hammingDistance' (unit k) (unit v) powers)) imbalanced
          imbalanced = (nodesToFix graph x delta extra)

nodesLeftOver :: Graph -> Pseudoflow -> Weight -> IndexList -> IndexList
nodesLeftOver graph x delta extra = leftovers s t
    where  s = (sSet graph x delta) A.++ (afst extra) A.++ (afst extra)
           t = (tSet graph x delta) A.++ (asnd extra) A.++ (asnd extra)

nodesToFix :: Graph -> Pseudoflow -> Weight -> IndexList -> Acc (Array DIM1 (InIndex,InIndex))
nodesToFix graph x delta extra = A.zip s t
    where  s = (sSet graph x delta) A.++ (afst extra) A.++ (afst extra)
           t = (tSet graph x delta) A.++ (asnd extra) A.++ (asnd extra)

leftovers :: Acc (Array DIM1 Int) -> Acc (Array DIM1 Int) -> IndexList
leftovers s t = acond
    (sl >* tl)
    (lift (A.drop tl s, empty))
    (lift (empty, A.drop sl t))
    where sl = A.size s
          tl = A.size t
          empty = A.filter (\x -> constant False) (toVect [1::Int])

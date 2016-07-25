{-# LANGUAGE FlexibleContexts #-}

module ReadDist where

import Data.Array.Accelerate as A
import System.Random
import System.IO
import System.Environment

normalize :: (RealFrac a) => [a] -> [a] -> ([a],[a])
normalize xs ys = (Prelude.map (*factor) xs, Prelude.map (*factor) ys)
    where factor = (Prelude.sum xs)/(Prelude.sum ys)

randomList :: (Random a) => (a,a) -> Int -> StdGen -> [a]
randomList bnds n = Prelude.take n . randomRs bnds

mapTuple :: (a -> b) -> (a,a) -> (b,b)
mapTuple f (a1,a2) = (f a1, f a2)

multList :: Int -> Int -> ([Int], [Int])
multList n i
    | diff >= 0 = (0:r1, diff:r2)
    | otherwise = ((-diff):r1, 0:r2)
    where r1 = Prelude.take (n-1) (randomList (0,n) (2*n) gen)
          r2 = Prelude.drop (n+1) (randomList (0,n) (2*n) gen)
          diff = Prelude.sum r1 - Prelude.sum r2
          gen = mkStdGen (6083387987 + i)

testList :: Int -> ([Int], [Int])
testList n
    | diff >= 0 = (0:r1, diff:r2)
    | otherwise = ((-diff):r1, 0:r2)
    where r1 = Prelude.take (n-1) (randomList (0,n) (2*n) gen)
          r2 = Prelude.drop (n+1) (randomList (0,n) (2*n) gen)
          diff = Prelude.sum r1 - Prelude.sum r2
          gen = mkStdGen 6083387987

revIntToBinary :: Int -> [Int]
revIntToBinary a
    | a <= 1 = [a]
    | a > 1  = (a `mod` 2):(revIntToBinary ((a - (a `mod` 2)) `div` 2))

intToBinary :: Int -> [Int]
intToBinary = Prelude.reverse . revIntToBinary

hammingDistance' :: Acc (Scalar Int) -> Acc (Scalar Int) -> Acc (Vector Int) -> Acc (Scalar Int)
hammingDistance' k v powers = A.sum $ A.zipWith (\ x y -> (x==*y) ? (0,1)) ks vs
   where ks = A.map (oneAt k) powers :: Acc (Vector Int)
         vs = A.map (oneAt v) powers :: Acc (Vector Int)

oneAt :: Acc (Scalar Int) -> Exp Int -> Exp Int
oneAt l = (\i -> ((the l) `mod` (2*i) >=* i ? (0,1)))

hammingDistance :: Int -> Int -> Int
hammingDistance a b = hammingListDistance as bs
    where as = Prelude.fst $ equalizeLength (intToBinary a) (intToBinary b)
          bs = Prelude.snd $ equalizeLength (intToBinary a) (intToBinary b)

equalizeLength :: [Int] -> [Int] -> ([Int], [Int])
equalizeLength x y
    | lx == ly = (x,y)
    | lx > ly  = equalizeLength x (0:y)
    | lx < ly  = equalizeLength (0:x) y
    where lx = Prelude.length x
          ly = Prelude.length y

hammingListDistance :: [Int] -> [Int] -> Int
hammingListDistance (x:xs) (y:ys) = abs (x - y) + hammingListDistance xs ys
hammingListDistance [] [] = 0

readLists :: String -> [[Int]]
readLists file = Prelude.map (\x -> Prelude.zipWith (-) (read $ (words x) Prelude.!! 0) (read $ (words x) Prelude.!! 1)) (lines file)

copyMatrix :: String -> Array DIM2 Word32 --Int
copyMatrix file = A.fromList sh list :: Array DIM2 Word32 --Int
    where sh = (Z:.dim:.dim)
          list = read $ (lines file) Prelude.!! 1
          dim = read $ (lines file) Prelude.!! 0 

generateMatrix :: Int -> Array DIM2 Int
generateMatrix a = A.fromList sh list :: Array DIM2 Int
    where sh = (Z:.a:.a) 
          list = [ hammingDistance n m | n <- [0..(a-1)], m <- [0..(a-1)]]

toVect :: (Elt a) => [a] -> Acc (Array DIM1 a)
toVect list = use (A.fromList sh list)
    where sh = (Z:.l)
          l  = Prelude.length list

--use the value seven
isInt :: (Integral a, RealFrac b) => b -> a -> Bool
isInt x n = (Prelude.round $ 10^(Prelude.fromIntegral n)*(x-(Prelude.fromIntegral $ Prelude.round x)))==0

toInt' :: (RealFrac a, Integral b) => a -> b
toInt' x = Prelude.floor $ head $ Prelude.filter (\i -> isInt (x*i) 7) $ Prelude.map Prelude.fromIntegral [1..]

toIntList' :: (RealFrac a, Integral b) => [a] -> [b]
toIntList' x = Prelude.map (Prelude.floor . ((multiplier' x)*)) x

multiplier' :: (RealFrac a) => [a] -> a
multiplier' x = Prelude.fromIntegral $ Prelude.foldr (lcm) 1 $ Prelude.map (toInt') x

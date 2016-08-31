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

multList :: Int -> Int -> ([Int], [Int])
multList n i
    | diff >= 0 = (0:r1, diff:r2)
    | otherwise = ((-diff):r1, 0:r2)
    where r1 = Prelude.take (n-1) (randomList (0,n) (2*n) gen)
          r2 = Prelude.drop (n+1) (randomList (0,n) (2*n) gen)
          diff = Prelude.sum r1 - Prelude.sum r2
          gen = mkStdGen (3012512025 + i)

testList :: Int -> ([Int], [Int])
testList n
    | diff >= 0 = (0:r1, diff:r2)
    | otherwise = ((-diff):r1, 0:r2)
    where r1 = Prelude.take (n-1) (randomList (0,n) (2*n) gen)
          r2 = Prelude.drop (n+1) (randomList (0,n) (2*n) gen)
          diff = Prelude.sum r1 - Prelude.sum r2
          gen = mkStdGen 3012512025

revIntToBinary :: Int -> [Int]
revIntToBinary a
    | a <= 1 = [a]
    | a > 1  = (a `mod` 2):(revIntToBinary ((a - (a `mod` 2)) `div` 2))

intToBinary :: Int -> [Int]
intToBinary = Prelude.reverse . revIntToBinary

powers :: Int -> [Int]
powers nodes = Prelude.map (2^) $ takeWhile (<=nodes) [1..]
--replace 15 with nodes in the future tbh

hammingDistance' :: Int -> Int -> [Int] -> Int
hammingDistance' k v p = Prelude.sum $ Prelude.zipWith (\ x y -> if (x==y) then 0 else 1) ks vs
   where ks = Prelude.map (oneAt k) p
         vs = Prelude.map (oneAt v) p

oneAt :: Int -> Int -> Int
oneAt l = (\i -> if  (l `mod` (2*i) >= i) then 1 else 0)

distance :: Int -> Int -> Int
distance = hammingDistance

-- | computes the hamming distance between two numbers, after converting them to binary
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

copyMatrix :: String -> Array DIM2 Word32 --Int
copyMatrix file = A.fromList sh list :: Array DIM2 Word32 --Int
    where sh = (Z:.dim:.dim)
          list = read $ (lines file) Prelude.!! 1
          dim = read $ (lines file) Prelude.!! 0 

-- | takes a list to an accelerate array
toVect :: (Elt a) => [a] -> Acc (Array DIM1 a)
toVect list = use (A.fromList sh list)
    where sh = (Z:.l)
          l  = Prelude.length list

--use the value seven for n
isInt :: (Integral a, RealFrac b) => b -> a -> Bool
isInt x n = (Prelude.round $ 10^(Prelude.fromIntegral n)*(x-(Prelude.fromIntegral $ Prelude.round x)))==0

denominator :: (RealFrac a, Integral b) => a -> b
denominator x = Prelude.floor $ head $ Prelude.filter (\i -> isInt (x*i) 7) $ Prelude.map Prelude.fromIntegral [1..]

toIntList' :: (RealFrac a, Integral b) => [a] -> [b]
toIntList' x = Prelude.map (Prelude.floor . ((multiplier' x)*)) x

multiplier' :: (RealFrac a) => [a] -> a
multiplier' x = Prelude.fromIntegral $ Prelude.foldr (lcm) 1 $ Prelude.map (denominator) x

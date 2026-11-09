{-# OPTIONS_GHC -Wall #-}
import Distribution.Simple.Program.HcPkg (list)
fiveToPower :: Integer -> Integer
fiveToPower x = (5^x)

toPower5 :: Num a => a -> a
toPower5 x = (x^5)

subtractFrom5 :: Num a => a -> a
subtractFrom5 x = subtract 5 x

add3C :: Num a => a -> a -> a -> a
add3C x y z = x + y + z

isPrime :: Int -> Bool
isPrime n = if (n < 2) then False else[i | i <- [2 .. n - 1], n `mod` i == 0] == []

fact :: (Ord t, Num t) => t -> t
fact n = if (n < 0) 
            then 1 
            else 
                if n == 0 || n == 1
                then 1
                else n * fact (n - 1)


fib :: Int -> Int
fib n = if n == 0 || n == 1
          then n
          else fib (n - 2) + fib (n - 1)

twoToPower :: (Integral b, Num a) => b -> a
twoToPower n = 2 ^ n

toPowerTwo :: Num a => a -> a
toPowerTwo n = n ^ 2

doubleList :: Num a => [a] -> [a]
doubleList []     = []              -- przypadek bazowy: pusta lista daje pustą listę
doubleList (x:xs) = (2 * x) : doubleList xs

areEven :: Integral a => [a] -> [Bool]
areEven list = go [] list
    where
        go acc [] = acc
        go acc (x:xs) = go (acc ++ [even x]) xs
        
areEven2 :: Integral a => [a] -> [Bool]
areEven2 [] = []
areEven2 (x:xs) = even x : areEven xs

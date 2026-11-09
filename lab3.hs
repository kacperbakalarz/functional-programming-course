{-# OPTIONS_GHC -Wall #-}
import Distribution.Simple.Utils (xargs)

add1,add2,add3 :: Num a => a -> a -> a
add1 = \x -> \y -> x+y
add2 x = \y -> x + y
add3 x y = x + y

lambdaTimesTwo :: Integer -> Integer
lambdaTimesTwo = \x -> x*2

lambdaToPowerTwo :: Integer -> Integer
lambdaToPowerTwo = \x -> x^2

lambdaTwoToPower :: Integer -> Integer
lambdaTwoToPower = \x -> 2^x

sumWith :: Num a => (a->a) -> [a] -> a
sumWith _ [] = 0
sumWith f (x:xs) = f x + sumWith f xs

applyTwice :: (t -> t) -> t -> t
applyTwice f x = f (f x)

applyN :: (Ord t1, Num t1) => t1 -> (t2 -> t2) -> t2 -> t2
applyN n f x
    | n <= 0 = x
    | otherwise = f(applyN (n-1) f x)




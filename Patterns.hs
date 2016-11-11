module Patterns where

import Data.Tuple

import NumberFields
import Euclid

-- This file is a playground to test the Euclidean algorithm on different classes of values

ints :: [Qi]
ints = map fromInteger [0..]

fibs :: [Qi]
fibs = map fromInteger . map snd $ iterate (\(x,y) -> (y,x+y)) (0,1)

-- { f(k) + f(k+n) i } for k = [0..]
offsetFibs :: Int -> [Qi]
offsetFibs n = zipWith (+) fibs (map (*i) (drop n fibs))

getPairs :: Int -> [a] -> [(a,a)]
getPairs n xs = (head xs, head . drop n $ xs) : getPairs n (tail xs)

pattern1 :: Int -> Int -> Int -> [(Qi,Qi)]
pattern1 upTo pairNum offsetNum = take upTo . map swap . getPairs pairNum $ offsetFibs offsetNum

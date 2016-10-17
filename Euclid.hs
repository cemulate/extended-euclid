module Euclid where

import System.Environment
import Text.Printf

-- A euclidean domain should have a euclidean function/algorithm:
class Euclidean a where
    quotientRemainder :: a -> a -> (a, a)

-- Generates the table / data structure associated with running the extended Euclidean algorithm
euclid :: (Num a, Eq a, Euclidean a) => a -> a -> [(a,a,a,a)]
euclid a b = eInner [(0, b, 0, 1), (0, a, 1, 0)] where
    -- Stop if previous remainder is 0:
    eInner cur@((_, 0, _, _):rest) = cur
    -- Compute next from last two previous and put it at the head of the list; recurse
    eInner cur@(sp:spp:rest) = eInner $ (eNext spp sp) : cur
    -- step function
    eNext (q1, r1, s1, t1) (q2, r2, s2, t2) = (q3, r3, s3, t3) where
        (q3, r3) = quotientRemainder r1 r2
        s3 = s1 - (q3 * s2)
        t3 = t1 - (q3 * t2)

pad :: Int -> String -> String
pad n s = s ++ take (n - length s) (repeat ' ')

-- Pretty-prints the result of 'euclid'
printSteps :: (Show a) => [(a,a,a,a)] -> String
printSteps rows = foldr (++) "" (reverse finalRows) where
    strRows = map (\(a,b,c,d) -> [show a, show b, show c, show d]) rows
    maxLength = maximum $ map length (concat strRows)
    padRows = map (map (pad maxLength)) strRows
    finalRows = map (\[a,b,c,d] -> "| " ++ a ++ " | " ++ b ++ " | " ++ c ++ " | " ++ d ++ " |\n") padRows

-- IO actions that output the result of running the Euclidean algorithm on the input pair:

-- (a,b) does b divided by a
doEuclid' :: (Num a, Eq a, Show a, Euclidean a) => (a,a) -> IO ()
doEuclid' = putStrLn . printSteps . uncurry euclid

-- (a,b) does a divided by b
doEuclid :: (Num a, Eq a, Show a, Euclidean a) => (a,a) -> IO ()
doEuclid = uncurry . flip . curry $ doEuclid'

-- For a list
doMultipleEuclid :: (Num a, Eq a, Show a, Euclidean a) => [(a,a)] -> IO ()
doMultipleEuclid = sequence_ . map doEuclid

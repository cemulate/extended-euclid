module Euclid where

import System.Environment
import Text.Printf

-- A euclidean domain should have a euclidean function/algorithm:
class Euclidean a where
    quotientRemainder :: a -> a -> (a, a)

-- Generates the table / data structure associated with running the extended Euclidean algorithm on a divided by b
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

-- Pretty-prints the result of 'euclid', can output (approximate) tex for pasting into arrays
printSteps :: (Show a) => Bool -> [(a,a,a,a)] -> String
printSteps tex rows = foldr (++) "" (reverse finalRows) where
    strRows = map (\(a,b,c,d) -> [show a, show b, show c, show d]) rows
    maxLength = maximum $ map length (concat strRows)
    padRows = map (map (pad maxLength)) strRows
    leftChar = if tex then "" else "| "
    sepChar = if tex then " & " else " | "
    rightChar = if tex then "\\\\\n" else " |\n"
    finalRows = map (\[a,b,c,d] -> leftChar ++ a ++ sepChar ++ b ++ sepChar ++ c ++ sepChar ++ d ++ rightChar) padRows

-- IO actions that output the result of running the Euclidean algorithm on the input pair:

-- (a,b) does b divided by a
doEuclid :: (Num a, Eq a, Show a, Euclidean a) => (a,a) -> IO ()
doEuclid = putStrLn . printSteps False . uncurry euclid

-- Utility to do the same but print tex
doEuclidTex :: (Num a, Eq a, Show a, Euclidean a) => (a,a) -> IO ()
doEuclidTex = putStrLn . printSteps True . uncurry euclid

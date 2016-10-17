module NumberFields where

import Euclid
import Text.Printf
import Data.Ratio

-- Instances of Num and (more importantly) Euclidean for some different number fields

-- Z (trivial)

instance Euclidean Integer where
    quotientRemainder = quotRem

-- Z[i] = Integers of Q(i)

data Qi = Qi Rational Rational deriving (Eq)

instance Show Qi where
    show (Qi a b) = sa ++ " + " ++ sb ++ "i" where
        (a1, a2, b1, b2) = (numerator a, denominator a, numerator b, denominator b)
        sa = if (a2 == 1) then (show a1) else printf "(%d/%d)" a1 a2
        sb = if (b2 == 1) then (show b1) else printf "(%d/%d)" b1 b2

instance Num Qi where
    fromInteger n = Qi (fromInteger n) 0
    negate (Qi a b) = Qi (-a) (-b)
    (Qi a b) + (Qi c d) = Qi (a+c) (b+d)
    (Qi a b) * (Qi c d) = Qi (a*c - b*d) (a*d + b*c)
    abs (Qi a b) = fromRational (a*a + b*b)

instance Fractional Qi where
    fromRational q = Qi q 0
    (Qi a b) / (Qi c d) = Qi ((a*c + b*d) / (c*c + d*d)) ((b*c - a*d) / (c*c + d*d))

i :: Qi
i = Qi 0 1

instance Euclidean Qi where
    quotientRemainder a b = (quot, rem) where
        (Qi qreal qimag) = a/b
        quot = Qi (fromInteger $ round qreal) (fromInteger $ round qimag)
        rem = a - quot*b

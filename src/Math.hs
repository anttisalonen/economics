module Math
where

type Flt = Double

tolerance :: (Ord a, Num a) => a -> a -> Bool
tolerance a d = abs a < d

diffTolerance :: (Ord a, Num a) => a -> a -> a -> Bool
diffTolerance a b d = abs (a - b) < d

invalidFloat :: (RealFloat a) => a -> Bool
invalidFloat f = isNaN f || isInfinite f || isNegativeZero f



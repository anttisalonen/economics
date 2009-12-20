module Math
where

type Flt = Double

tolerance :: (Ord a, Num a) => a -> a -> Bool
tolerance a d = abs a < d

diffTolerance :: (Ord a, Num a) => a -> a -> a -> Bool
diffTolerance a b d = abs (a - b) < d

newton :: (Fractional a) => (a -> a) -> (a -> a) -> a -> a
newton f df x0 =
  let a = f x0
      b = df x0
      c = x0 - (a / b)
  in c

newtonIter
  :: (Fractional a, Ord a, Num b, Ord b) =>
     (a -> a) -> (a -> a) -> a -> b -> a -> a
newtonIter f df eps cycles x0 
  | cycles <= 0 = x0
  | otherwise =
    let x1 = newton f df x0
    in if abs (x1 - x0) < eps 
         then x1
         else newtonIter f df eps (cycles - 1) x1

type Polynomial a = [(a, a)]

derivePolynomial :: (Num a) => Polynomial a -> Polynomial a
derivePolynomial = filter (\(c, e) -> c /= 0) . map (\(c, e) -> (e * c, e - 1))

polynomial :: (Floating a) => a -> Polynomial a -> a
polynomial x = sum . map (\(c, e) -> c * (x ** e))

valuePolynomial :: (Floating a) => Polynomial a -> a -> a
valuePolynomial = flip polynomial

invalidFloat :: (RealFloat a) => a -> Bool
invalidFloat f = isNaN f || isInfinite f || isNegativeZero f


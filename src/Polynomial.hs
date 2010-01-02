module Polynomial(newton, newtonIter,
    mkPol, simplify, cleanPol,
    root, derivePolynomial,
    polynomial, valuePolynomial,
    Polynomial(..))
where

import Data.List
import Data.Function (on)

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

newtype Polynomial a = Polynomial { getPol :: [(a, a)] }
  deriving (Eq, Read, Show)

flipPol :: ([(a, a)] -> [(a, a)]) -> Polynomial a -> Polynomial a
flipPol f = Polynomial . f . getPol

mkPol :: (Ord a, Num a) => [(a, a)] -> Polynomial a
mkPol = cleanPol . simplify . Polynomial . sortBy (compare `on` snd)

simplify :: (Eq a, Num a) => Polynomial a -> Polynomial a
simplify p = 
  let simplifyterm x = (((foldl (flip $ (+) . fst)) 0) x, (snd . head) x)
      grouped = groupBy ((==) `on` snd) (getPol p)
  in Polynomial (cleanPol' $ map simplifyterm grouped)
             -- in zipWith (,) (map ((foldl (flip $ (+) . fst)) 0) grouped) (map (snd . head) grouped)

cleanPol :: (Num a, Eq a) => Polynomial a -> Polynomial a
cleanPol = flipPol cleanPol'

cleanPol' :: (Num a, Eq a) => [(a, a)] -> [(a, a)]
cleanPol' = filter (\(c, e) -> c /= 0)

root :: (Ord a, Floating a) => Polynomial a -> a
root p = newtonIter (valuePolynomial p) (valuePolynomial (derivePolynomial p)) 0.0001 (10 :: Int) 1

derivePolynomial :: (Num a) => Polynomial a -> Polynomial a
derivePolynomial = flipPol (cleanPol' . map (\(c, e) -> (e * c, e - 1)))

polynomial :: (Floating a) => a -> Polynomial a -> a
polynomial x = sum . map (\(c, e) -> c * (x ** e)) . getPol

valuePolynomial :: (Floating a) => Polynomial a -> a -> a
valuePolynomial = flip polynomial

multTerm :: (Num a) => (a, a) -> (a, a) -> (a, a)
multTerm (c1, e1) (c2, e2) = (c1 * c2, e1 + e2)

multPol :: (Num a) => [(a, a)] -> [(a, a)] -> [(a, a)]
multPol p1 p2 = cleanPol' [x `multTerm` y | x <- p1, y <- p2]

absPol :: (Num a) => Polynomial a -> Polynomial a
absPol = flipPol (map (\(c, e) -> (abs c, e)))

signumPol :: (Num a) => Polynomial a -> Polynomial a
signumPol p = flipPol (signum . fst . head) (simplify p)

negatePol :: (Num a) => Polynomial a -> Polynomial a
negatePol = flipPol (map (\(c, e) -> (negate c, e)))

instance (Eq a, Ord a, Num a) => Num (Polynomial a) where
  p1 + p2 = mkPol (getPol p1 ++ getPol p2)
  negate = negatePol
  p1 * p2 = simplify $ mkPol (getPol p1 `multPol` getPol p2)
  abs = absPol
  signum = signumPol
  fromInteger x = mkPol [(fromInteger x, 0)]


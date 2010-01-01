{-# LANGUAGE TypeSynonymInstances #-}
module Curve
where

import Data.List (foldl')

import Libaddutil.Misc (quadr)

import Math
import Polynomial

type Curve = [CurveComponent]

data CurveComponent = LinearFunction Flt Flt
                    | QuadraticFunction Flt Flt Flt
                    | ExponentialFunction Flt Flt Flt
    deriving (Eq, Show, Read)

maxCurveValue :: Flt
maxCurveValue = 1e7

mkCurve = (:[])

lookupX :: Curve -> Flt -> Flt
lookupX cs x = sum $ (map (\c -> lookupX' c x)) cs

lookupX' :: CurveComponent -> Flt -> Flt
lookupX' (LinearFunction a b) y 
 | a == 0    = maxCurveValue
 | otherwise = ((y - b) / a)
lookupX' (QuadraticFunction a b c) y = 
  let val = quadr a b (y - c)
  in if null val 
       then 0
       else maximum val
lookupX' (ExponentialFunction a b c) y 
 | a == 0     = maxCurveValue
 | b == 0     = maxCurveValue
 | y - c == 0 = maxCurveValue
 | otherwise  = (((y - c) / b) ** (1 / a))

lookupY :: Curve -> Flt -> Flt
lookupY cs y = sum $ (map (\c -> lookupY' c y)) cs

lookupY' :: CurveComponent -> Flt -> Flt
lookupY' (LinearFunction a b)      x = a * x + b
lookupY' (QuadraticFunction a b c) x = a * (x ^ (2 :: Int)) + b * x + c
lookupY' (ExponentialFunction a b c) x 
 | x == 0 && a == 0 = c
 | otherwise        = b * (x ** a) + c

curveToPol :: Curve -> Polynomial Flt
curveToPol = mkPol . concatMap curveToPol'

curveToPol' :: CurveComponent -> [(Flt, Flt)]
curveToPol' (LinearFunction a b)        = [(a, 1), (b, 0)]
curveToPol' (QuadraticFunction a b c)   = [(a, 2), (b, 1), (c, 0)]
curveToPol' (ExponentialFunction a b c) = [(b, a), (c, 0)]

polToCurve :: Polynomial Flt -> Curve
polToCurve p1 = foldl' go [] (getPol p1)
  where go acc (c, e) | e == 0    = LinearFunction 0 c:acc
                      | e == 1    = LinearFunction c 0:acc
                      | otherwise = ExponentialFunction e c 0:acc

balance' :: Polynomial Flt -> Polynomial Flt -> Maybe (Flt, Flt)
balance' x y = 
  let xy = mkPol $ (getPol x) ++ map (\(a, b) -> (-a, b)) (getPol y)
      xy' = derivePolynomial xy
      q = newtonIter (valuePolynomial xy) (valuePolynomial xy') 0.0001 (10 :: Int) 1
      p = polynomial q x
  in if q <= 0 || p <= 0 || invalidFloat q || invalidFloat p 
       then Nothing 
       else Just (q, polynomial q x)

balance :: Curve -> Curve -> Maybe (Flt, Flt) -- (Quantity, Price)
balance c1 c2 = balance' (curveToPol c1) (curveToPol c2)

pcp c1 c2 f = polToCurve (f (curveToPol c1) (curveToPol c2))

instance Num Curve where
  c1 + c2 = pcp c1 c2 (+)
  c1 - c2 = pcp c1 c2 (-)
  c1 * c2 = pcp c1 c2 (*)
  negate = polToCurve . negate . curveToPol
  abs = polToCurve . abs . curveToPol
  signum = polToCurve . signum . curveToPol
  fromInteger = polToCurve . fromInteger


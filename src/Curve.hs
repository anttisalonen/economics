module Curve
where

import Types

import Libaddutil.Misc (quadr)

data Curve = LinearFunction Flt Flt
           | QuadraticFunction Flt Flt Flt
           | ExponentialFunction Flt Flt Flt
    deriving (Eq, Show, Read)

maxvalue :: Flt
maxvalue = 1e17

lookupX :: Curve -> Flt -> Flt
lookupX (LinearFunction a b) y 
 | a == 0    = maxvalue
 | otherwise = (y - b) / a
lookupX (QuadraticFunction a b c) y = 
  let val = quadr a b (y - c)
  in if null val 
       then 0 
       else maximum val
lookupX (ExponentialFunction a b c) y 
 | a == 0     = maxvalue
 | b == 0     = maxvalue
 | y - c == 0 = maxvalue
 | otherwise  = ((y - c) / b) ** (1 / a)

lookupY :: Curve -> Flt -> Flt
lookupY (LinearFunction a b)      x = a * x + b
lookupY (QuadraticFunction a b c) x = a * (x ^ (2 :: Int)) + b * x + c
lookupY (ExponentialFunction a b c) x 
 | x == 0 && a == 0 = c
 | otherwise        = b * (x ** a) + c


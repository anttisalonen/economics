module SD
where

import Data.Maybe

import Math
import Line

type Elasticity = Flt

newtype Supply = Supply { linsupply :: Line }
  deriving (Eq, Read, Show)
newtype Demand = Demand { lindemand :: Line }
  deriving (Eq, Read, Show)

class Curve a where
  fromLine :: Line -> a
  toLine :: a -> Line

instance Curve Supply where
  fromLine = Supply
  toLine = linsupply

instance Curve Demand where
  fromLine = Demand
  toLine = lindemand

mkLinear :: (Curve a) => Flt -> Flt -> a
mkLinear a' b' = fromLine (Line a' b')

mkLinearFromEP :: (Curve a) => Elasticity -> Point2 -> a
mkLinearFromEP e (q, p) = 
  let b' = e * q / p
      a' = q - b' * p
  in mkLinear a' b'

isValidDemand :: (Curve a) => a -> Bool
isValidDemand c = 
  priceAtAmount c 0 > 0 && amountAtPrice c 0 > 0 && b (toLine c) <= 0

isValidSupply :: (Curve a) => a -> Bool
isValidSupply c =
  priceAtAmount c 0 < 0 && b (toLine c) >= 0

priceElasticity :: (Curve a) => a -> Flt -> Elasticity
priceElasticity c q =
  (p / q) * (b (toLine c))
    where p = priceAtAmount c q

priceAtAmount :: (Curve a) => a -> Flt -> Flt
priceAtAmount = invLineFunc . toLine

amountAtPrice :: (Curve a) => a -> Flt -> Flt
amountAtPrice = lineFunc . toLine

balance :: Supply -> Demand -> Maybe Point2
balance s d = 
  let a' = a $ toLine s
      b' = b $ toLine s
      c' = a $ toLine d
      d' = b $ toLine d
      q = (a' - c') / (d' - b')
      p = a' + b' * q
  in if q > 0 && p > 0 then Just (q, p) else Nothing



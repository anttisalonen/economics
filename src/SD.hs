module SD
where

import Data.Maybe
import Test.QuickCheck
import Test.HUnit

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

instance Arbitrary Supply where
  arbitrary = do
    a' <- choose (1,100 :: Int)
    b' <- choose (1,100 :: Int)
    return $ mkLinear (fromIntegral a') (fromIntegral b')

instance Arbitrary Demand where
  arbitrary = do
    a' <- choose (1,100 :: Int)
    b' <- choose (-100,-1 :: Int)
    return $ mkLinear (fromIntegral a') (fromIntegral b')

prop_isSupply :: Supply -> Bool
prop_isSupply = isValidSupply

prop_isDemand :: Demand -> Bool
prop_isDemand = isValidDemand

prop_mklinearFromEP1 :: Elasticity -> Point2 -> Property
prop_mklinearFromEP1 e n@(q, p) = e > 0 && q > 0 && p > 0 ==>
  let c = mkLinearFromEP e n :: Supply
  in isValidSupply c ==> diffTolerance (priceElasticity c q) e 0.00001

prop_mklinearFromEP2 :: Elasticity -> Point2 -> Property
prop_mklinearFromEP2 e n@(q, p) = e < 0 && q > 0 && p > 0 ==>
  let c = mkLinearFromEP e n :: Demand
  in isValidDemand c ==> diffTolerance (priceElasticity c q) e 0.00001

prop_mklinearFromEP3 :: Supply -> Flt -> Property
prop_mklinearFromEP3 c q =
  let e = priceElasticity c q
      p = amountAtPrice c q
      c' = mkLinearFromEP e (q, p) :: Supply
  in q > 0 && p > 0 && e > 0 ==> diffTolerance (amountAtPrice c' p) q 0.00001

prop_mklinearFromEP4 :: Demand -> Flt -> Property
prop_mklinearFromEP4 c q =
  let e = priceElasticity c q
      p = priceAtAmount c q
      c' = mkLinearFromEP e (q, p) :: Demand
  in q > 0 && e < 0 && p > 0 ==> isValidDemand c' && diffTolerance (amountAtPrice c' p) q 0.00001

prop_elasticity1 :: Supply -> Flt -> Property
prop_elasticity1 s q = q > 0 ==>
  let e = priceElasticity s q
      s' = mkLinearFromEP e ((q, priceAtAmount s q)) :: Supply
      a1 = a (toLine s)
      b1 = b (toLine s)
      a2 = a (toLine s')
      b2 = b (toLine s')
  in e > 0 ==> diffTolerance a1 a2 0.00001 &&
               diffTolerance b1 b2 0.00001

prop_elasticity2 :: Demand -> Flt -> Property
prop_elasticity2 s q = q > 0 ==>
  let e = priceElasticity s q
      s' = mkLinearFromEP e ((q, priceAtAmount s q)) :: Demand
      a1 = a (toLine s)
      b1 = b (toLine s)
      a2 = a (toLine s')
      b2 = b (toLine s')
  in e < 0 ==> diffTolerance a1 a2 0.00001 &&
               diffTolerance b1 b2 0.00001

prop_balance :: Supply -> Demand -> Property
prop_balance s d = 
  amountAtPrice s 0 < amountAtPrice d 0 ==> isJust $ balance s d

test24 = 
  let s = mkLinear 1800 240
      d = mkLinear 3550 (-266)
      mb = balance s d
      (p, q) = fromJust mb
  in do 
    assertBool "Balance" (isJust mb)
    assertBool ("Price: " ++ show p) (p > 3.45 && p < 3.47)
    assertBool ("Quantity: " ++ show q) (floor q == 2630)


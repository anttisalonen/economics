module SDTest
where

import Data.Maybe
import Test.QuickCheck
import Test.HUnit

import Math
import Line
import SD

instance Arbitrary Supply where
  arbitrary = do
    a' <- choose (1,1000 :: Int)
    b' <- choose (1,1000 :: Int)
    return $ mkLinear (fromIntegral a') (fromIntegral b')

instance Arbitrary Demand where
  arbitrary = do
    a' <- choose (1,1000 :: Int)
    b' <- choose (-1000,-1 :: Int)
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
    assertBool ("Quantity: " ++ show q) (floor q == (2630 :: Int))

test26 =
  let p = 0.75
      q = 7.5
      es = 1.6
      ed = -0.8
      s = mkLinearFromEP es (q, p) :: Supply
      d = mkLinearFromEP ed (q, p) :: Demand
      sl = toLine s
      dl = toLine d
  in do
    assertBool "Supply" (a sl == -4.5 && b sl == 16)
    assertBool "Demand" (a dl == 13.5 && b dl == -8)


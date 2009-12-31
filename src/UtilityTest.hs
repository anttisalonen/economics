module UtilityTest
where

import Test.HUnit
import Test.QuickCheck

import TestUtils
import Utility
import Types

test4a4 = do
  let uf = CobbDouglas 0.5
  let px = 1
  let py = 3
  let i = 100
  let (df1, df2) = demandCurve uf i px py
  let qx = demandQuantity df1 px
  let qy = demandQuantity df2 py
  assertBool ("Quantity (X): " ++ show qx) (closeEnough qx 50 0.0001)
  assertBool ("Quantity (Y): " ++ show qy) (closeEnough qy 16.6666 0.0001)

instance Arbitrary UtilityFunction where
  arbitrary = do
    t <- choose (1, 3 :: Int)
    cda <- choose (0, 1 :: Flt)
    coeffa <- choose (0, 100 :: Flt)
    case t of
      0 -> return $ CobbDouglas cda
      1 -> return $ Substitute coeffa
      _ -> return $ Complement coeffa

prop_factors1 :: UtilityFunction -> Price -> Price -> Flt -> Flt -> Property
prop_factors1 uf px py i1 i2 = px > 0 && py > 0 && i1 > 0 && i1 < i2 ==> factors uf px py i1 < factors uf px py i2


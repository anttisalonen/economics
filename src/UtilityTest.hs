module UtilityTest
where

import Test.HUnit

import Utility

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
  

module ProductionTest
where

import Test.HUnit

import TestUtils
import Curve
import Production
import Cost

-- Pindyck, Rubinfeld, 2003: Chaptel 7, assignment 2.
test72 = 
  let prodfunc = CobbDouglas 100 1 1
      q        = 1000
      r        = 120
      w        = 30
      costfunc = (0, prodfunc)
      c        = cost costfunc r w q
      (k, l)   = factors prodfunc r w q
      q'       = production prodfunc k l
  in do 
    assertBool ("Capital: " ++ show k) (closeEnough k (sqrt 2.5) 0.001)
    assertBool ("Labor: " ++ show l) (closeEnough l ((sqrt 2.5) * 4) 0.001)
    assertBool ("Cost: " ++ show c) (closeEnough c 379.20 1)
    assertBool ("Production: " ++ show q') (closeEnough q 1000 0.001)

test73 =
  let p = CobbDouglas 1 1 2
      r = 10
      w = 15
      (k, l) = factors p r w 1000
  in do
    assertBool ("Capital/Labor ratio: " ++ show (k / l)) (closeEnough (k / l) 0.75 0.001)

test83 =
  let fc = 100
      mc = LinearFunction 2 0
      pf = CobbDouglas (sqrt 2) 0.25 0.25
      cf = (fc, pf)
      r = 1
      w = 1
      mcf = marginalCosts cf r w
      p = 60
      q = productionQuantity mcf p
      q' = productionQuantity mc p
      c = totalCosts cf r w q
      q2 = productionQuantity mcf 0
      q3 = productionQuantity mcf 1
  in do
    assertBool ("Production (Linear): " ++ show q) (closeEnough q 30 0.0001)
    assertBool ("Production (Cobb-Douglas): " ++ show q') (closeEnough q' 30 0.0001)
    assertBool ("Costs: " ++ show c) (closeEnough c 1000 0.0001)
    assertBool ("Production at price 0: " ++ show q2) (closeEnough q2 0 0.0001)
    assertBool ("Production at price 1: " ++ show q3) (closeEnough q3 0.5 0.0001)

test86 =
  let mc = LinearFunction 2 3
      p  = 9
      q  = productionQuantity mc p
  in do
    assertBool ("Production: " ++ show q) (closeEnough q 3 0.0001)



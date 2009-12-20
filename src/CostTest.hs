module CostTest
where

import Test.HUnit

import Curve
import Cost
import Production

testComplement = do
  let p = Complement 1 4
  assertBool ("Marginal costs 1") (marginalCosts' p 1 1 == LinearFunction 5 0)
  assertBool ("Marginal costs 2") (marginalCosts' p 1 8 == LinearFunction 12 0)
  assertBool ("Marginal costs 3") (marginalCosts' p 8 1 == LinearFunction 33 0)
  assertBool ("Cost 1") (cost (0, p) 1 1 100 == 500)
  assertBool ("Cost 2") (cost (0, p) 1 8 100 == 1200)
  assertBool ("Cost 3") (cost (0, p) 8 1 100 == 3300)
  assertBool ("Factors 1") (factors p 1 1 100 == (400,100))
  assertBool ("Factors 2") (factors p 8 1 100 == (400,100))
  assertBool ("Factors 3") (factors p 1 8 100 == (400,100))
  assertBool ("Production 1") (production p 8 1 == 1)
  assertBool ("Production 2") (production p 1 8 == 0.25)
  assertBool ("Production 3") (production p 16 4 == 4)

testSubstitute = do
  let p = Substitute 1 4
  assertBool ("Marginal costs 1") (marginalCosts' p 3 3 == LinearFunction 3 0)
  assertBool ("Marginal costs 2") (marginalCosts' p 3 8 == LinearFunction 3 0)
  assertBool ("Marginal costs 3") (marginalCosts' p 8 3 == LinearFunction 8 0)
  assertBool ("Marginal costs 4") (marginalCosts' p 8 8 == LinearFunction 8 0)
  assertBool ("Cost 1") (cost (0, p) 3 3 100 == 300)
  assertBool ("Cost 2") (cost (0, p) 30 3 100 == 300)
  assertBool ("Cost 3") (cost (0, p) 3 30 100 == 300)
  assertBool ("Cost 4") (cost (0, p) 6 6 100 == 600)
  assertBool ("Factors 1") (factors p 3 3 100 == (100,0))
  assertBool ("Factors 2") (factors p 30 3 100 == (0,100))
  assertBool ("Factors 3") (factors p 3 30 100 == (100,0))
  assertBool ("Factors 4") (factors p 30 30 100 == (100,0))
  assertBool ("Production 1") (production p 3 3 == 15)
  assertBool ("Production 2") (production p 30 3 == 123)
  assertBool ("Production 3") (production p 3 30 == 42)
  assertBool ("Production 4") (production p 30 30 == 150)


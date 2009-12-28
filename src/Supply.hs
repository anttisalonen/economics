module Supply(balance)
where

import Math
import Types
import Curve

curveToPol :: Curve -> Polynomial Flt
curveToPol (LinearFunction a b)        = [(a, 1), (b, 0)]
curveToPol (QuadraticFunction a b c)   = [(a, 2), (b, 1), (c, 0)]
curveToPol (ExponentialFunction a b c) = [(b, a), (c, 0)]

balance' :: Polynomial Flt -> Polynomial Flt -> Maybe (Flt, Flt)
balance' x y = 
  let xy = x ++ map (\(a, b) -> (-a, b)) y
      xy' = derivePolynomial xy
      q = newtonIter (valuePolynomial xy) (valuePolynomial xy') 0.0001 (10 :: Int) 1
      p = polynomial q x
  in if q <= 0 || p <= 0 || invalidFloat q || invalidFloat p 
       then Nothing 
       else Just (q, polynomial q x)

balance :: Curve -> Curve -> Maybe (Quantity, Price)
balance c1 c2 = balance' (curveToPol c1) (curveToPol c2)


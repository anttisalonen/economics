module Utility
where

import Types
import Curve

cobbDouglasUtility :: Flt -> Flt -> Flt -> Flt
cobbDouglasUtility a x y = a * log x + (1 - a) * log y

cobbDouglasDemand :: Flt -> ((Flt -> Price -> Quantity), (Flt -> Price -> Quantity))
cobbDouglasDemand a = (dx, dy)
  where dx = \i px -> if px == 0 then 1 / maxCurveValue else (a / px) * i
        dy = \i py -> if py == 0 then 1 / maxCurveValue else ((1 - a) / py) * i

cobbDouglasDemand' :: Flt -> Flt -> ((Price -> Quantity), (Price -> Quantity))
cobbDouglasDemand' a i = 
  let (dx, dy) = cobbDouglasDemand a
  in (dx i, dy i)

sqUtility :: Flt -> Flt -> Flt
sqUtility = cobbDouglasUtility 0.5

sqDemand :: ((Flt -> Price -> Quantity), (Flt -> Price -> Quantity))
sqDemand = cobbDouglasDemand 0.5

sqDemand' :: Flt -> (Price -> Quantity)
sqDemand' = fst . cobbDouglasDemand' 0.5

perfectSubstitute :: Flt -> Flt -> Flt -> Flt
perfectSubstitute a x y = a * x + y

sub1 :: Flt -> Flt -> Flt -> Bool
sub1 a px py = px <= a * py

perfectSubstituteDemand :: ((Flt -> Flt -> Price -> Price -> Quantity), (Flt -> Flt -> Price -> Price -> Quantity))
perfectSubstituteDemand = (dx, dy)
  where dx = \a i py px -> if sub1 a px py       then if px == 0 then 1 / maxCurveValue else i / px else 0
        dy = \a i px py -> if not (sub1 a px py) then if py == 0 then 1 / maxCurveValue else i / py else 0

perfectSubstituteDemand' :: Flt -> Flt -> ((Price -> Price -> Quantity), (Price -> Price -> Quantity))
perfectSubstituteDemand' a i = 
  let (dx, dy) = perfectSubstituteDemand
  in (dx a i, dy a i)

perfectComplementUtility :: Flt -> Flt -> Flt -> Flt
perfectComplementUtility a x y = min (a * y) x

perfectComplementDemand :: ((Flt -> Flt -> Price -> Price -> Quantity), (Flt -> Flt -> Price -> Price -> Quantity))
perfectComplementDemand = (dx, dy)
  where dx = \a i py px -> if (px * a + py) == 0 then 1 / maxCurveValue else i * a / (px * a + py)
        dy = \a i px py -> if (px * a + py) == 0 then 1 / maxCurveValue else i     / (px * a + py)

perfectComplementDemand' :: Flt -> Flt -> ((Price -> Price -> Quantity), (Price -> Price -> Quantity))
perfectComplementDemand' a i = 
  let (dx, dy) = perfectComplementDemand
  in (dx a i, dy a i)

data UtilityFunction = CobbDouglas { alpha :: Flt }
                     | Substitute { alpha :: Flt }
                     | Complement { alpha :: Flt }
    deriving (Eq, Show, Read)

factors :: UtilityFunction -> Price -> Price -> Flt -> (Quantity, Quantity)
factors (CobbDouglas a) px py i = (fst (cobbDouglasDemand' a i) px, snd (cobbDouglasDemand' a i) py)
factors (Substitute a)  px py i = 
  let (dx, dy) = perfectSubstituteDemand' a i
  in  (dx py px, dy px py)
factors (Complement a)  px py i =
  let (dx, dy) = perfectComplementDemand' a i
  in  (dx py px, dy px py)

factorsL :: UtilityFunction -> [Price] -> Flt -> [Quantity]
factorsL uf [p1, p2] i =
  let (x, y) = factors uf p1 p2 i
  in [x, y]
factorsL _ _ _ = error "TODO: utility factors for multiple inputs not defined"

demandCurve :: UtilityFunction -> Price -> Price -> Price -> (DemandCurve, DemandCurve)
demandCurve (CobbDouglas a) i _ _ =
  (mkCurve $ ExponentialFunction (-1) (a * i) 0, mkCurve $ ExponentialFunction (-1) ((1 - a) * i) 0)
demandCurve (Substitute a) i px py =
  let prefer1 = sub1 a px py
      c1 = mkCurve $ ExponentialFunction (-1) i 0
      c2 = mkCurve $ LinearFunction maxCurveValue 0
      cx = if prefer1     then c1 else c2
      cy = if not prefer1 then c1 else c2
  in (cx, cy)
demandCurve (Complement a) i _ _ =
  (mkCurve $ ExponentialFunction (-1) (a * i) 0, mkCurve $ ExponentialFunction (-1) (if a == 0 then 1 / maxCurveValue else (i / a)) 0)

demandQuantity :: DemandCurve -> Price -> Quantity
demandQuantity = lookupX

demandPrice :: DemandCurve -> Quantity -> Price
demandPrice = lookupY

utility :: UtilityFunction -> Quantity -> Quantity -> Flt
utility (CobbDouglas a) = cobbDouglasUtility a
utility (Substitute a)  = perfectSubstitute a
utility (Complement a)  = perfectComplementUtility a

utilityL :: UtilityFunction -> [Quantity] -> Flt
utilityL uf [q1, q2] = utility uf q1 q2
utilityL _ _ = error "TODO: utility for multiple inputs not defined"


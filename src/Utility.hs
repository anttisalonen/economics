module Utility
where

import Types
import Curve

cobbDouglasUtility :: Flt -> Flt -> Flt -> Flt
cobbDouglasUtility a x y = a * log x + (1 - a) * log y

cobbDouglasDemand :: Flt -> ((Flt -> Price -> Quantity), (Flt -> Price -> Quantity))
cobbDouglasDemand a = (dx, dy)
  where dx = \i px -> (a / px) * i
        dy = \i py -> ((1 - a) / py) * i

cobbDouglasDemand' :: Flt -> Flt -> ((Price -> Quantity), (Price -> Quantity))
cobbDouglasDemand' a i = 
  let (dx, dy) = cobbDouglasDemand a
  in (dx i, dy i)

cobbDouglasDemand1 :: Flt -> Flt -> Price -> Quantity
cobbDouglasDemand1 a i px = (a / px) * i

cobbDouglasDemand2 :: Flt -> Flt -> Price -> Quantity
cobbDouglasDemand2 a i py = ((1 - a) / py) * i

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
  where dx = \a i py px -> if sub1 a px py       then i / px else 0
        dy = \a i px py -> if not (sub1 a px py) then i / py else 0

perfectSubstituteDemand' :: Flt -> Flt -> ((Price -> Price -> Quantity), (Price -> Price -> Quantity))
perfectSubstituteDemand' a i = 
  let (dx, dy) = perfectSubstituteDemand
  in (dx a i, dy a i)

perfectComplementUtility :: Flt -> Flt -> Flt -> Flt
perfectComplementUtility a x y = min (a * x) y

perfectComplementDemand :: ((Flt -> Flt -> Price -> Price -> Quantity), (Flt -> Flt -> Price -> Price -> Quantity))
perfectComplementDemand = (dx, dy)
  where dx = \a i py px -> i / (max (a * px) py)
        dy = \a i px py -> i / (max (a * px) py)

perfectComplementDemand' :: Flt -> Flt -> ((Price -> Price -> Quantity), (Price -> Price -> Quantity))
perfectComplementDemand' a i = 
  let (dx, dy) = perfectComplementDemand
  in (dx a i, dy a i)

data UtilityFunction = CobbDouglas { alpha :: Flt }
                     | Substitute { alpha :: Flt }
                     | Complement { alpha :: Flt }
    deriving (Eq, Show, Read)

factors :: UtilityFunction -> Price -> Price -> Flt -> (Flt, Flt)
factors (CobbDouglas a) px py i = (cobbDouglasDemand1 a i px, cobbDouglasDemand2 a i py)
factors (Substitute a)  px py i = 
  let (dx, dy) = perfectSubstituteDemand' a i
  in  (dx py px, dy px py)
factors (Complement a)  px py i =
  let (dx, dy) = perfectComplementDemand' a i
  in  (dx py px, dy px py)

demandCurve :: UtilityFunction -> Price -> Price -> Price -> (DemandCurve, DemandCurve)
demandCurve (CobbDouglas a) i _ _ =
  (ExponentialFunction (-1) (a * i) 0, ExponentialFunction (-1) ((1 - a) * i) 0)
demandCurve (Substitute a) i px py =
  let prefer1 = sub1 a px py
      c1 = ExponentialFunction (-1) i 0
      c2 = LinearFunction (1 / 0) 0
      cx = if prefer1     then c1 else c2
      cy = if not prefer1 then c1 else c2
  in (cx, cy)
demandCurve (Complement a) i _ _ =
  (ExponentialFunction (-1) (a * i) 0, ExponentialFunction (-1) (i / a) 0)

demandQuantity :: DemandCurve -> Price -> Quantity
demandQuantity = lookupX


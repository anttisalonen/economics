module Utility
where

import Math

cobbDouglas :: Flt -> Flt -> Flt -> Flt
cobbDouglas a x y = a * log x + (1 - a) * log y

cobbDouglasDemand :: Flt -> ((Flt -> Flt -> Flt), (Flt -> Flt -> Flt))
cobbDouglasDemand a = (dx, dy)
  where dx = \px i -> (a / px) * i
        dy = \py i -> (a / py) * i

sqUtility :: Flt -> Flt -> Flt
sqUtility = cobbDouglas 0.5

sqDemand :: ((Flt -> Flt -> Flt), (Flt -> Flt -> Flt))
sqDemand = cobbDouglasDemand 0.5

perfectSubstitute :: Flt -> Flt -> Flt
perfectSubstitute = (+)

perfectSubstituteDemand :: ((Flt -> Flt -> Flt -> Flt), (Flt -> Flt -> Flt -> Flt))
perfectSubstituteDemand = (dx, dy)
  where dx = \px py i -> if px <= py then i / px else 0
        dy = \py px i -> if py <  px then i / py else 0

perfectComplement :: Flt -> Flt -> Flt
perfectComplement = min

perfectComplementDemand :: ((Flt -> Flt -> Flt -> Flt), (Flt -> Flt -> Flt -> Flt))
perfectComplementDemand = (dx, dy)
  where dx = \px py i -> i / (max px py)
        dy = \py px i -> i / (max px py)


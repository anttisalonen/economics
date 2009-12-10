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

perfectComplement :: Flt -> Flt -> Flt
perfectComplement = min

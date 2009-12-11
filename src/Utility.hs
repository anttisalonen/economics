module Utility
where

import Math
import Types

cobbDouglasUtility :: Flt -> Flt -> Flt -> Flt
cobbDouglasUtility a x y = a * log x + (1 - a) * log y

cobbDouglasDemand :: Flt -> ((Flt -> Price -> Quantity), (Flt -> Price -> Quantity))
cobbDouglasDemand a = (dx, dy)
  where dx = \i px -> (a / px) * i
        dy = \i py -> ((1 - a) / py) * i

cobbDouglasDemand' :: Flt -> Flt -> ((Price -> Quantity), (Price -> Quantity))
cobbDouglasDemand' a i = (dx, dy)
  where dx = \px -> (a / px) * i
        dy = \py -> ((1 - a) / py) * i

sqUtility :: Flt -> Flt -> Flt
sqUtility = cobbDouglasUtility 0.5

sqDemand :: ((Flt -> Price -> Quantity), (Flt -> Price -> Quantity))
sqDemand = cobbDouglasDemand 0.5

sqDemand' :: Flt -> (Price -> Quantity)
sqDemand' = fst . cobbDouglasDemand' 0.5

perfectSubstitute :: Flt -> Flt -> Flt
perfectSubstitute = (+)

perfectSubstituteDemand :: ((Flt -> Price -> Price -> Quantity), (Flt -> Price -> Price -> Quantity))
perfectSubstituteDemand = (dx, dy)
  where dx = \i py px -> if px <= py then i / px else 0
        dy = \i px py -> if py <  px then i / py else 0

perfectSubstituteDemand' :: Flt -> ((Price -> Price -> Quantity), (Price -> Price -> Quantity))
perfectSubstituteDemand' i = (dx, dy)
  where dx = \py px -> if px <= py then i / px else 0
        dy = \px py -> if py <  px then i / py else 0

perfectComplementUtility :: Flt -> Flt -> Flt
perfectComplementUtility = min

perfectComplementDemand :: ((Flt -> Price -> Price -> Quantity), (Flt -> Price -> Price -> Quantity))
perfectComplementDemand = (dx, dy)
  where dx = \i py px -> i / (max px py)
        dy = \i px py -> i / (max px py)

perfectComplementDemand' :: Flt -> ((Price -> Price -> Quantity), (Price -> Price -> Quantity))
perfectComplementDemand' i = (dx, dy)
  where dx = \py px -> i / (max px py)
        dy = \px py -> i / (max px py)


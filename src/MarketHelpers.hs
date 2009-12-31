module MarketHelpers
where

-- import Text.Regex(subRegex, mkRegex)

import qualified Data.Edison.Assoc.StandardMap as E
import qualified Data.Edison.Assoc as EX

import qualified Production as P
import qualified Utility as U
import Types
import Cost
import Curve
import MarketTypes

mkEconomy :: Price -> ProductionMap -> UtilityMap -> ProductName -> MarketQuantityMap -> Economy
mkEconomy b ps u rn reg = Economy b ps u rn (E.fromSeq []) (E.fromSeq []) reg

tendLin :: (Num a, Ord a) => a -> a -> a -> a
tendLin maxchange start target =
  let diff   = target - start
      change = if diff > 0
                 then min maxchange diff
                 else max (-maxchange) diff
  in start + change

tendLog :: (Num a, Ord a) => a -> a -> a -> a
tendLog coeff start target =
  let diff   = target - start
      change = diff * coeff
  in start + change

buildProductMap :: [(ProductName, a)] -> ProductMap a
buildProductMap = E.fromSeq

productionToUtility :: P.ProductionFunction -> U.UtilityFunction
productionToUtility (P.CobbDouglas _ a b) = U.CobbDouglas (a / (a + b))
productionToUtility (P.Substitute _ c)    = U.Substitute c
productionToUtility (P.Complement _ c)    = U.Complement c

productionInfoToUtilityInfo :: ProductionInfo -> UtilityInfo
productionInfoToUtilityInfo (ProductionInfo pfunc i1 i2 _ _) = UtilityInfo (productionToUtility pfunc) i1 i2

split :: (a, (b, c)) -> ((a, b), (a, c))
split (a, (b, c)) = ((a, b), (a, c))

splitMap :: (EX.Assoc m k) => m (a, b) -> (m a, m b)
splitMap m = (EX.fromList (zip keys fsts), EX.fromList (zip keys snds))
  where (keys, values) = unzip $ EX.toList m
        fsts = map fst values
        snds = map snd values

-- | Creates the demand curves for a good. The demand curves depend on the
-- price of the good as well as budget restriction and the utility info 
-- values.
mkDemand :: MarketPriceMap -> Price -> UtilityInfo -> [(ProductName, DemandCurve)]
mkDemand ps i (UtilityInfo uf o1 o2) =
  let p1 = E.lookupWithDefault maxCurveValue o1 ps
      p2 = E.lookupWithDefault maxCurveValue o2 ps
      (d1, d2) = U.demandCurve uf i p1 p2
  in [(o1, d1), (o2, d2)]

-- | Creates a supply curve for a good. The supply curve depends on the price
-- on the market as well as the production info values.
mkSupply :: MarketPriceMap -> ProductName -> ProductionInfo -> [(ProductName, SupplyCurve)]
mkSupply ps pn (ProductionInfo pf i1 i2 maxchange changecoeff) =
  let p1 = E.lookupWithDefault maxCurveValue i1 ps
      p2 = E.lookupWithDefault maxCurveValue i2 ps
  in [(pn, marginalCosts' pf p1 p2)]

-- | Given current quantity, change coefficients and supply and demand
-- curves, will return the new quantity produced by supply based on
-- the new price determined by the demand of the given quantity.
--
-- The resulting quantity q' will tend to q as given by coefficients.
--
-- P^
--  |
--  |\_                    /
--  |  \__ demand         /
--  |     \_           __/  supply
--  |        \_     __/
--  |           \_ /  
--  |            _/\__
--  |        ___/      \__
--p'|--------------------->\__
--  |  __/  ^              ^  \__
--  | /     |              |     \
--  +-------------------------------------------->
--          q'             q                     Q
--
updateQP :: Quantity -> Flt -> Flt -> SupplyCurve -> DemandCurve -> (Quantity, Price)
updateQP q change coeff s d = (q', p')
  where p' = lookupX d q
        q' = tendLin change q $ tendLog coeff q $ lookupY s p'

{-
ppTuple :: (PrintfArg a, PrintfArg b) => (a, b) -> String
ppTuple (a, b) = printf "%3.3f %3.3f" a b

ppTuple2 (a, b) = ppTuple a ++ "\t" ++ ppTuple b

ppTuple4 :: (PrintfArg a, PrintfArg b) => (a, b) -> String
ppTuple4 (a, b) = printf "%3.2f,%3.2f" a b

ppTuple5 :: (Show a, Show b, Show c, Show d,
             PrintfArg a,
             PrintfArg b,
             PrintfArg c,
             PrintfArg d) => ((a, b), (c, d)) -> String
ppTuple5 (a, b) = ppTuple4 a ++ "," ++ ppTuple4 b
-}

-- substitute x with y in s
-- substitute :: String -> String -> String -> String
-- substitute x y s = subRegex (mkRegex x) s y

-- A chart:
-- > import Utility
-- > import Graphics.Rendering.Chart.Simple
-- > plotWindow [20,21..80.0] (Utility.demandQuantity d1) (Cost.productionQuantity' pf1 100 1)


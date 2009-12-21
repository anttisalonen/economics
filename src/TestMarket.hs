module TestMarket
where

import Text.Printf
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
-- import Text.Regex(subRegex, mkRegex)

import qualified Data.Edison.Assoc.StandardMap as E
import qualified Data.Edison.Seq.SimpleQueue as S

import qualified Production as P
import qualified Utility as U
import Types
import Cost
import Curve

type ProductName = String

type ProductMap a = E.FM ProductName a

data ProductionInfo = ProductionInfo { productionfunction :: P.ProductionFunction,
                                       input1             :: ProductName,
                                       input2             :: ProductName,
                                       maxchange          :: Flt,
                                       changecoeff        :: Flt
                                     }

type ProductionMap = ProductMap ProductionInfo

data UtilityInfo = UtilityInfo { utilityfuncion :: U.UtilityFunction
                               , output1        :: ProductName
                               , output2        :: ProductName
                               }

type UtilityMap = ProductMap UtilityInfo

type MarketQuantityMap = ProductMap Quantity

type MarketPriceMap = ProductMap Price

type MarketSupplyMap = ProductMap SupplyCurve

type MarketDemandMap = ProductMap DemandCurve

pf1 = P.CobbDouglas 1 0.25 0.25
pf2 = P.Substitute  1 1

rental = 1000
wage = 1
i = 100

uf1 = U.CobbDouglas 0.5

productionmap = E.fromSeq [("Food",     ProductionInfo pf1 "Labor" "Capital" 10 0.1),
                           ("Clothing", ProductionInfo pf2 "Labor" "Capital" 10 0.1)]

utilitymap = E.fromSeq [("Welfare", UtilityInfo uf1 "Food" "Clothing")]

quantitymap = E.fromSeq [("Food", 0.0), ("Clothing", 0.0)]

pricemap = E.fromSeq [("Food", 0.0), ("Clothing", 0.0)]

klpricemap = E.fromSeq [("Labor", wage), ("Capital", rental)]

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

run (p1, p2) (q1, q2) = 
  let (demand1, demand2) = U.demandCurve uf1 i p1 p2
      supply1            = marginalCosts' pf1 rental wage
      supply2            = marginalCosts' pf2 rental wage
      np1                = lookupX demand1 q1
      np2                = lookupX demand2 q2
      nq1                = tendLin 10 q1 $ tendLog 0.1 q1 $ lookupY supply1 np1
      nq2                = tendLin 10 q2 $ tendLog 0.1 q2 $ lookupY supply2 np2
  in ((np1, np2), (nq1, nq2))

buildProductMap :: [(ProductName, a)] -> ProductMap a
buildProductMap = E.fromSeq

updateQP :: Quantity -> Flt -> Flt -> SupplyCurve -> DemandCurve -> (Quantity, Price)
updateQP q change coeff s d = (q', p')
  where p' = lookupX d q
        q' = tendLin change q $ tendLog coeff q $ lookupY s p'

split :: (a, (b, c)) -> ((a, b), (a, c))
split (a, (b, c)) = ((a, b), (a, c))

mkDemand :: MarketPriceMap -> Price -> UtilityInfo -> [(ProductName, DemandCurve)]
mkDemand ps i (UtilityInfo uf o1 o2) =
  let p1 = E.lookupWithDefault maxCurveValue o1 ps
      p2 = E.lookupWithDefault maxCurveValue o2 ps
      (d1, d2) = U.demandCurve uf i p1 p2
  in [(o1, d1), (o2, d2)]

mkSupply :: MarketPriceMap -> ProductName -> ProductionInfo -> [(ProductName, SupplyCurve)]
mkSupply ps pn (ProductionInfo pf i1 i2 maxchange changecoeff) =
  let p1 = E.lookupWithDefault maxCurveValue i1 ps
      p2 = E.lookupWithDefault maxCurveValue i2 ps
  in [(pn, marginalCosts' pf p1 p2)]

gather :: ProductionMap -> MarketQuantityMap -> MarketSupplyMap -> MarketDemandMap -> (MarketQuantityMap, MarketPriceMap)
gather prods quantities supplies demands = 
 E.foldWithKey' (adjustMarket quantities supplies demands) (E.empty, E.empty) prods

adjustMarket :: MarketQuantityMap                    -- | Quantities available on market
             -> MarketSupplyMap                      -- | Market supply curves
             -> MarketDemandMap                      -- | Market demand curves
             -> ProductName                          -- | Name of product to adjust
             -> ProductionInfo                       -- | Production function info
             -> (MarketQuantityMap, MarketPriceMap)  -- | Market info
             -> (MarketQuantityMap, MarketPriceMap)  -- | Market info
adjustMarket quantities supplies demands name prod (qs, ps) = fromMaybe (qs, ps) $ do
          let q = E.lookupWithDefault 0 name quantities
          let change = maxchange prod
          let coeff = changecoeff prod
          s <- E.lookupM name supplies
          d <- E.lookupM name demands
          let (q', p') = updateQP q change coeff s d
          return (E.insert name q' qs, E.insert name p' ps)

run' :: ProductionMap -> UtilityMap -> Price -> MarketQuantityMap -> MarketPriceMap -> (MarketQuantityMap, MarketPriceMap)
run' prods utilities i quantities prices = 
  let demands  = buildProductMap $ concatMap (mkDemand prices i) (E.elements utilities)
      supplies = buildProductMap $ concatMap (uncurry (mkSupply prices)) (E.toSeq prods)
  in gather prods quantities supplies demands

runKL' :: ProductionMap -> UtilityMap -> MarketPriceMap -> Price -> MarketQuantityMap -> MarketPriceMap -> (MarketQuantityMap, MarketPriceMap)
runKL' prods utilities newprices i quantities prices = run' prods utilities i quantities (E.union newprices prices)

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

showMarket = mapM_ putStrLn . map ppTuple5 $ runMarket

runMarket = take 30 . drop 2 $ iterate (uncurry run) ((0, 0), (0, 0))

runMarket' = take 30 . drop 2 $ iterate (uncurry (runKL' productionmap utilitymap klpricemap i)) (quantitymap, pricemap)

showMarket' = putStrLn . concatMap showMarketInfo $ runMarket'

showMarketInfo :: (MarketQuantityMap, MarketPriceMap) -> String
showMarketInfo (qs, ps) = intercalate "\n" . S.toList . S.rcons "\n" $ E.foldWithKey' go (S.singleton (printf "%-21s %-12s %s" "Name" "Quantity" "Price")) qs
  where go :: ProductName -> Quantity -> S.Seq String -> S.Seq String
        go name q prev = let mp = E.lookupM name ps
                         in case mp of
                              Nothing -> prev
                              Just p  -> S.rcons (printf "%-20s %9.2f %9.2f" name q p) prev

-- substitute x with y in s
-- substitute :: String -> String -> String -> String
-- substitute x y s = subRegex (mkRegex x) s y

-- A chart:
-- > import Utility
-- > import Graphics.Rendering.Chart.Simple
-- > plotWindow [20,21..80.0] (Utility.demandQuantity d1) (Cost.productionQuantity' pf1 100 1)


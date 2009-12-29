module ProductionTree(requiredInput)
where

import qualified Data.Edison.Assoc.StandardMap as E

import Libaddutil.BinTree

import qualified Production as P
import Cost
import Types
import Curve
import MarketTypes

-- Given a product to produce, market prices and a production map, return a 
-- list of quantities of input required for production.
requiredInput :: ProductionMap -> ProductName -> MarketPriceMap -> Maybe MarketQuantityMap
requiredInput = f' E.empty

f' :: MarketQuantityMap -> ProductionMap -> ProductName -> MarketPriceMap -> Maybe MarketQuantityMap
f' acc _ "" _ = Just acc
f' acc productions name prices = do
  p <- E.lookupM name productions
  let getprice x = if null x then 0 else E.lookupWithDefault maxCurveValue x prices
  let price      = getprice name
  let i1       = input1 p
  let i2       = input2 p
  let ip1      = getprice i1
  let ip2      = getprice i2
  let pf       = productionfunction p
  let curve    = marginalCosts' pf ip1 ip2
  let (f1, f2) = P.factors pf ip1 ip2 (productionQuantity curve price)
  t1 <- if null i1 
          then return acc 
          else f' (E.insertWith (+) i1 (f1 * ip1) acc) productions i1 prices
  if null i2 
    then return t1 
    else f' (E.insertWith (+) i2 (f2 * ip2) t1) productions i2 prices


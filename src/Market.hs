module Market
where

import Text.Printf
import Data.List (intercalate)
import Data.Maybe (fromMaybe)

import qualified Data.Edison.Assoc.StandardMap as E
import qualified Data.Edison.Seq.SimpleQueue as S

import qualified Production as P
import qualified Utility as U
import Types
import Cost
import Curve
import MarketHelpers
import MarketTypes

showEconomy :: Economy -> String
showEconomy e = 
  let (qm, pm) = splitMap (market e)
  in showMarketInfo (qm, pm)

showMarketInfo :: (MarketQuantityMap, MarketPriceMap) -> String
showMarketInfo (qs, ps) = intercalate "\n" . S.toList . S.rcons "\n" $ E.foldWithKey' go (S.singleton (printf "%-21s %-12s %s" "Name" "Quantity" "Price")) qs
  where go :: ProductName -> Quantity -> S.Seq String -> S.Seq String
        go name q prev = let mp = E.lookupM name ps
                         in case mp of
                              Nothing -> prev
                              Just p  -> S.rcons (printf "%-20s %9.2f %9.2f" name q p) prev

stepEconomy :: Economy -> Economy
stepEconomy e = 
  let oldBudget  = budget e
      oldProd    = production e
      oldUtility = utility e
      oldMarket  = market e
      newBudget  = oldBudget
      newUtility = oldUtility
      (market', prod')    = stepDemand oldUtility oldProd (getMarketPriceMap e) oldBudget oldMarket -- demand sets prices, production demands inputs
      (market'', newProd) = stepProd prod' oldMarket  -- supply supplies products
      newMarket  = stepMarket market'' -- demand consumes goods
  in Economy newBudget newProd newUtility newMarket

getMarketQuantityMap :: Economy -> MarketQuantityMap
getMarketQuantityMap = fst . splitMap . market

getMarketPriceMap :: Economy -> MarketPriceMap
getMarketPriceMap = snd . splitMap . market

stepDemand :: UtilityMap -> ProductionCentreMap -> MarketPriceMap -> Price -> MarketMap -> (MarketMap, ProductionCentreMap)
stepDemand utilities prods prices i ms = 
  let demands = buildProductMap $ concatMap (mkDemand prices i) (E.elements utilities)
  in (ms, prods)

stepProd :: ProductionCentreMap -> MarketMap -> (MarketMap, ProductionCentreMap)
stepProd prods ms = (ms, prods)

stepMarket :: MarketMap -> MarketMap
stepMarket ms = ms

runKL :: ProductionMap -> UtilityMap -> MarketPriceMap -> Price -> MarketQuantityMap -> MarketPriceMap -> (MarketQuantityMap, MarketPriceMap)
runKL prods utilities newprices i quantities prices = run prods utilities i quantities (E.union newprices prices)

-- | Creates a map of quantities and prices on the market. It is created
-- based on the production map, quantities on the market (for tending)
-- and on the demand and supply curves. The curves are created based on
-- the budget constraint, prices and utility functions for demand,
-- prices and production infos for supply.
run :: ProductionMap -> UtilityMap -> Price -> MarketQuantityMap -> MarketPriceMap -> (MarketQuantityMap, MarketPriceMap)
run prods utilities i quantities prices = 
  let demands  = buildProductMap $ concatMap (mkDemand prices i) (E.elements utilities)
      supplies = buildProductMap $ concatMap (uncurry (mkSupply prices)) (E.toSeq prods)
  in gather prods quantities supplies demands

-- | Creates a map of quantities and prices on the market. All the production
-- infos are processed - i.e. they produce goods depending on the current
-- quantities on the market. The new quantities and prices are adjusted
-- according to the supply and demand curves.
gather :: ProductionMap -> MarketQuantityMap -> MarketSupplyMap -> MarketDemandMap -> (MarketQuantityMap, MarketPriceMap)
gather prods quantities supplies demands = 
 E.foldWithKey' (adjustMarket quantities supplies demands) (E.empty, E.empty) prods

-- | Updates market info (tuple of quantity and price maps) by changing the
-- price and quantity in the market info. The new quantity and price tend
-- towards equilibrium. They are dependent on the supply and demand curves.
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



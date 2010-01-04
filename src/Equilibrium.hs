module Equilibrium
where

import Data.Maybe (fromMaybe)

import qualified Data.Edison.Assoc.StandardMap as E

import Market
import MarketTypes
import Types
import Curve

step1 :: Economy -> Economy
step1 e = 
  let demands = marketDemands e
      supplies = marketSupplies e
      (qs', ps') = E.foldrWithKey' (create supplies) (E.empty, E.empty) demands
  in e{marketquantity = qs', marketprice = ps'}

create :: MarketSupplyMap -> ProductName -> DemandCurve -> (MarketQuantityMap, MarketPriceMap) -> (MarketQuantityMap, MarketPriceMap)
create supplies pname d acc@(accq, accp) = fromMaybe acc $ do
  s <- E.lookupM pname supplies
  (q, p) <- balance s d
  return (E.insert pname q accq, E.insert pname p accp)


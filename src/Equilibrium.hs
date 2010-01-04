module Equilibrium
where

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
create supplies pname d acc@(accq, accp) =
  case E.lookupM pname supplies of
    Nothing -> acc
    Just s  -> let bal = balance s d
               in case bal of
                    Nothing     -> acc
                    Just (q, p) -> (E.insert pname q accq, E.insert pname p accp)


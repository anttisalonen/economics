module MarketTransaction
where

import qualified Data.Edison.Assoc.StandardMap as E

import Cost
import Types
import Curve
import MarketTypes

withdraw :: MarketQuantityMap -> MarketQuantityMap -> (MarketQuantityMap, MarketQuantityMap)
withdraw shoppinglist market = E.foldrWithKey' go (E.empty, market) shoppinglist
  where go name q (accb, accm) = let tq = min q (E.lookupWithDefault 0 name accm)
                                 in (E.insertWith (+) name tq accb, E.insertWith (subtract) name tq accm)

deposit :: MarketQuantityMap -> MarketQuantityMap -> MarketQuantityMap
deposit list market = E.foldrWithKey' go market list
  where go name q accm = E.insertWith (+) name q accm


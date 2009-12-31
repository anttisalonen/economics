module MarketTransaction
where

import qualified Data.Edison.Assoc.StandardMap as E

import MarketTypes

-- Given a list of things to withdraw and the current market state, returns
-- the new market state without the withdrawn goods, plus a map of quantities
-- of goods that could actually be bought.
withdraw :: MarketQuantityMap -> MarketQuantityMap -> (MarketQuantityMap, MarketQuantityMap)
withdraw shoppinglist market = E.foldrWithKey' go (market, E.empty) shoppinglist
  where go name q (accm, accb) = let curr = E.lookupWithDefault 0 name accm
                                     transq = min curr q
                                 in (E.insertWith subtract name transq accm, E.insertWith (+) name transq accb)

-- Given a list of things to add to the market and the current market state,
-- adds the things to the market and returns the updated market.
deposit :: MarketQuantityMap -> MarketQuantityMap -> MarketQuantityMap
deposit list market = E.foldrWithKey' go market list
  where go name q accm = E.insertWith (+) name q accm

consume :: MarketQuantityMap -> MarketQuantityMap -> MarketQuantityMap
consume s m = fst $ withdraw s m


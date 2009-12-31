module UtilityTree(
    weightAllocation, budgetAllocation,
    resolveAllocation, quantityAllocation,
    buildUtilityTree,
    UtilityTree, MultiplicatorTree)
where

import qualified Data.Edison.Assoc.StandardMap as E

import Libaddutil.BinTree

import qualified Utility as U
import Types
import MarketTypes

weightAllocation :: UtilityTree -> MultiplicatorTree
weightAllocation = g . f

budgetAllocation :: Price -> UtilityTree -> MarketPriceMap
budgetAllocation b = resolveAllocation b . weightAllocation

quantityAllocation :: MarketPriceMap -> MarketPriceMap -> MarketQuantityMap
quantityAllocation prices budgets = E.mapWithKey func budgets
  where func k b = case E.lookupM k prices of
                     Nothing -> 0
                     Just p  -> b / p

buildUtilityTree :: ProductName -> UtilityMap -> MarketPriceMap -> UtilityTree
buildUtilityTree rootname utilities prices = 
  let val = E.lookupM rootname utilities
  in case val of
       Nothing
         -> LeafR (rootname, E.lookup rootname prices)
       Just (UtilityInfo uf o1 o2) 
         -> NodeR (rootname, uf) 
                (buildUtilityTree o1 utilities prices) 
                (buildUtilityTree o2 utilities prices)

f :: UtilityTree -> MiddleTree
f t@(NodeR (n, u) l r) = NodeR (n, u, p) (f l) (f r)
  where p = getPrice t
f (LeafR np) = LeafR np
f _ = error "Invalid utility tree: converting empty to price tree"

-- Price of a node. Trivial for leaves.
-- For inner nodes the prices of their children have to be resolved first.
-- This is done by going further down the tree; leaves already have their
-- prices, the inner nodes compute their own price by combining the prices
-- of their children and the utility function.
getPrice :: UtilityTree -> Price
getPrice t@(NodeR (_, u) l r) = 
  let (c1, c2) = getMultiplicators t
      p1       = getPrice l
      p2       = getPrice r
  in c1 * p1 + c2 * p2
getPrice (LeafR (_, p)) = p
getPrice _               = error "Invalid utility tree: reading price from empty"

getMultiplicators :: UtilityTree -> (Flt, Flt)
getMultiplicators (NodeR (_, u) l r) = (c1, c2)
  where c1 = q1 / (q1 + q2)
        c2 = q2 / (q1 + q2)
        (q1, q2) = U.factors u p1 p2 1
        p1 = getPrice l
        p2 = getPrice r
getMultiplicators _ = error "Invalid utility tree: reading multiplicators from non-node"

g :: MiddleTree -> MultiplicatorTree
g t = go 1 t
  where go v (NodeR (n, u, _) l r) = Node (n, v) (go g1 l) (go g2 r)
             where g1 = v1 / (v1 + v2)
                   g2 = v2 / (v1 + v2)
                   (v1, v2) = U.factors u (get l) (get r) 1
        go v (LeafR (n, _)) = Node (n, v) Empty Empty
        go _ EmptyR         = Empty

get :: MiddleTree -> Flt
get (NodeR (_, _, p) _ _) = p
get (LeafR (_, p))        = p
get EmptyR                = 0

-- Converts a final tree of only multiplicators to a map of quantities.
-- The multiplicators are simply multiplied all the way down.
resolveAllocation :: Flt -> MultiplicatorTree -> MarketPriceMap
resolveAllocation m (Node (n, v) Empty Empty) = E.singleton n (m * v)
resolveAllocation m (Node (_, v) l r)         = resolveAllocation (m * v) l `E.union` resolveAllocation (m * v) r
resolveAllocation _ Empty                     = E.empty


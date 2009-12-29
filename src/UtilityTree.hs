module UtilityTree(
    weightAllocation, budgetAllocation,
    UtilityTree, MultiplicatorTree)
where

import qualified Data.Edison.Assoc.StandardMap as E
import qualified Data.Foldable as Foldable

import Libaddutil.BinTree

import qualified Utility as U
import Types
import MarketTypes

type UtilityTree = BinTreeR (ProductName, U.UtilityFunction) (ProductName, Price)

type MiddleTree = BinTreeR (ProductName, U.UtilityFunction, Price) (ProductName, Price)

type MultiplicatorTree = BinTree (ProductName, Flt)

weightAllocation :: UtilityTree -> MultiplicatorTree
weightAllocation = g . f

budgetAllocation :: Price -> UtilityTree -> MarketPriceMap
budgetAllocation b = resolve b . weightAllocation

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
resolve :: Flt -> MultiplicatorTree -> MarketPriceMap
resolve m (Node (n, v) Empty Empty) = E.singleton n (m * v)
resolve m (Node (_, v) l r)         = resolve (m * v) l `E.union` resolve (m * v) r
resolve _ Empty                     = E.empty

mul :: Flt -> MultiplicatorTree -> MultiplicatorTree
mul v = fmap (\(x, p) -> (x, v * p))

toMap :: (Ord k) => BinTree (k, a) -> E.FM k a
toMap = E.fromSeq . Foldable.toList

{-
getE :: UtilityTree -> Either UtilityFunction Price
getE (Node e _ _) = e
getE (Leaf e)     = e
getE _            = error "Invalid utility tree: inner node has no child"
-}

{-
data UTree = Node ProductName UTree UTree | Leaf ProductName

a = Node "Welfare" 
      (Node "Food" 
        (Node "Meat" 
          (Leaf "Pork") 
          (Node "OtherMeat" 
            (Leaf "Beef") 
            (Leaf "Mutton"))) 
        (Node "Vegetables" 
          (Leaf "Wheat") 
          (Leaf "Rice")))
      (Node "Clothes" 
        (Leaf "Leather") 
        (Leaf "Wool"))

-- Utility with outputs being real goods on the market.
-- Result is map of product name to percentage of budget for allocation.
-- Percentage is multiplied with given coefficient.
f :: UtilityFunction -> ProductName -> ProductName -> MarketPriceMap -> Float -> MarketQuantityMap
f uf o1 o2 prices coeff = E.fromSeq [(o1, coeff * c1), (o2, coeff * c2)]
  where (q1, q2) = U.factors uf p1 p2 1
        c1       = q1 / (q1 + q2)
        c2       = q2 / (q1 + q2)
        p1       = E.lookupWithDefault maxCurveValue o1 prices
        p2       = E.lookupWithDefault maxCurveValue o2 prices
-}

{-
-- Original: inner nodes have utility functions, leaves have prices.
-- Utility functions and prices of children map to multiplicators for
-- the leaves, prices for the inner nodes.
type T1 = BinTreeR (ProductName, UtilityFunction) (ProductName, Price)

-- After first mapping: inner nodes have prices, leaves have multiplicators.
-- Mapped to everything having multiplicators.
type T2 = BinTreeR (ProductName, Price) (ProductName, Flt)

-- Final tree: multiplicators everywhere.
type T3 = BinTreeR Flt (ProductName, Flt)

-- Name of node (T1, T2).
value :: BinTreeR (String, a) (String, b) -> String
value (NodeR (n, _) _ _) = n
value (LeafR (n, _))     = n
value _                  = error "Invalid utility tree: inner node has no child"

-- Price of a node in T1. Trivial for leaves.
-- For inner nodes the prices of their children have to be resolved first.
-- This is done by going further down the tree; leaves already have their
-- prices, the inner nodes compute their own price by combining the prices
-- of their children and the utility function.
getPrice :: T1 -> Price
getPrice (LeafR (_, p))  = p
getPrice t@(NodeR u l r) = 
  let (c1, c2) = getMultiplicators t
      p1       = getPrice l
      p2       = getPrice r
  in c1 * p1 + c2 * p2
getPrice _               = error "Invalid utility tree: inner node has no child"

getMultiplicators :: T1 -> (Flt, Flt)
getMultiplicators (NodeR u l r) = (c1, c2)
  where c1 = q1 / (q1 + q2)
        c2 = q2 / (q1 + q2)
        (q1, q2) = U.factors u p1 p2
        p1 = getPrice l
        p2 = getPrice r
getMultiplicators _ = error "Invalid utility tree: inner node has no child"

-- Converts a tree with utility functions and prices to a tree of prices and
-- multiplicators. The inner nodes calculate their price by inspecting the
-- prices of their children and their utility function. The leaves are given
-- the multiplicators that resulted from the inspection.
f :: T1 -> T2
f t@(NodeR (n, u) l r) = Node (n, p) c1 c2
  where p        = getPrice t
        (c1, c2) = getMultiplicators t

-- Converts a final tree of only multiplicators to a map of quantities.
-- The multiplicators are simply multiplied all the way down.
g :: Flt -> T3 -> MarketQuantityMap
g m (NodeR v l r)  = g (m * v) l `E.union` g (m * v) r
g m (LeafR (n, x)) = E.singleton n (m * x)
g _ EmptyR         = E.empty

-- Converts a tree after first mapping to the final tree. The prices in inner
-- nodes are turned into multiplicators.
h :: T2 -> T3
h t@(NodeR (n, p) l r) = 
-}

{-
getPrice :: T1 -> Price
getPrice (LeafR (_, p))  = p
getPrice t@(NodeR u l r) = 
  let (q1, q2) = U.factors u p1 p2 1
      p1       = getPrice l
      p2       = getPrice r
      c1       = q1 / (q1 + q2)
      c2       = q2 / (q1 + q2)
  in c1 * p1 + c2 * p2
getPrice _               = error "Invalid utility tree: inner node has no child"
-}

{-
f :: UtilityFunction -> Price -> Price -> (Flt, Flt)
f uf p1 p2 = 
  let (q1, q2) = U.factors uf p1 p2 1
  in  (q1 / (q1 + q2), q2 / (q1 + q2))

g :: UtilityFunction -> Node -> Node -> Flt
g uf n1 n2 = f uf (getPrice n1) (getPrice n2)

g :: UtilityFunction -> UtilityMap -> ProductName -> ProductName -> MarketPriceMap -> Float -> MarketQuantityMap
g uf utilities o1 o2 prices coeff = 

getCoeff pn utilities prices = 
  case E.lookupM pn utilities of
    Nothing -> error "Item utility not found"
    Just u  -> let (q1, q2) = U.factors (utilityfunction u) (output1 u) (output2 u) i
               in E.fromSeq [(output1 u, q1), (output2 u, q2)]

f :: [ProductName] -> UtilityFunction -> UtilityMap -> MarketPriceMap -> Price -> MarketQuantityMap
f pnames uf utilities prices budget =
  let compcoeffs = map getCoeff pnames
  in  h uf compcoeffs budget

g :: ProductName -> UtilityMap -> MarketPriceMap -> Flt
g pn utilities prices =
  let u = E.lookup pn utilities
      o1 = output1 u
      o2 = output2 u
      p1 = getCoeff o1 utilities prices
      p2 = getCoeff o2 utilities prices
      ps = [p1, p2]
      qs = U.factorsL (utilityfunction u) ps 1
  in -- wieviel muss man zahlen, um Nutzen von 1 zu bekommen? utilityL (utilityfunction u) qs

-- | Maximizes the utility by choosing how to spend budget. The product name
-- should be the root utility.
spendBudget :: ProductName -> UtilityMap -> MarketPriceMap -> Price -> MarketQuantityMap
spendBudget pn utilities prices i =
  let thisU = E.lookupM pn utilities
  in case thisU of
       Just u  -> spendBudget' u utilities prices i
       Nothing -> E.fromSeq [(pn, i)]

spendBudget' :: UtilityInfo -> UtilityMap -> MarketPriceMap -> Price -> MarketQuantityMap
spendBudget' u utilities prices i = 
  let (qx, qy) = U.factors thisUf px py i
      thisUf   = utilityfunction u
      o1       = output1 u
      o2       = output2 u
      px       = getPrice o1 prices
      py       = getPrice o2 prices
      fx       = spendBudget o1 utilities prices (qx * px)
      fy       = spendBudget o2 utilities prices (qy * py)
  in fx `E.union` fy

-- | Gets price from price map.
getPrice :: ProductName -> MarketPriceMap -> Price
getPrice = E.lookupWithDefault maxCurveValue
-}

-- Test prices:
-- let prices = (E.fromSeq [("Beef", 20), ("Leather", 10), ("Mutton", 18), ("Pork", 15), ("Rice", 3), ("Wheat", 5), ("Wool", 8.0)])


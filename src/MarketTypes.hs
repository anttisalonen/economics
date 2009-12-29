module MarketTypes
where

import qualified Data.Edison.Assoc.StandardMap as E

import qualified Production as P
import qualified Utility as U
import Types
import Cost
import Curve

type ProductName = String

type ProductMap a = E.FM ProductName a

data ProductionCentre = ProductionCentre { productioninfo :: ProductionInfo
                                         , input1quantity :: Flt
                                         , input2quantity :: Flt
                                         }
    deriving (Eq, Show, Read)

data ProductionInfo = ProductionInfo { productionfunction :: P.ProductionFunction,
                                       input1             :: ProductName,
                                       input2             :: ProductName,
                                       maxchange          :: Flt,
                                       changecoeff        :: Flt
                                     }
    deriving (Eq, Show, Read)

type ProductionCentreMap = ProductMap ProductionCentre

type ProductionMap = ProductMap ProductionInfo

data UtilityInfo = UtilityInfo { utilityfunction :: U.UtilityFunction
                               , output1         :: ProductName
                               , output2         :: ProductName
                               }
    deriving (Eq, Show, Read)

type UtilityMap = ProductMap UtilityInfo

type MarketQuantityMap = ProductMap Quantity

type MarketPriceMap = ProductMap Price

type MarketSupplyMap = ProductMap SupplyCurve

type MarketDemandMap = ProductMap DemandCurve

type MarketMap = ProductMap (Quantity, Price)

data Economy = Economy { budget     :: Flt
                       , production :: ProductionCentreMap
                       , utility    :: UtilityMap
                       , market     :: MarketMap
                       }
    deriving (Eq, Show, Read)



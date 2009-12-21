module Types(Elasticity, Price, Quantity, Flt,
             Wage, Rental, Capital, Labor,
             DemandCurve, SupplyCurve, MarginalCostFunction)
where

import Math
import Curve

type Elasticity = Flt
type Price = Flt
type Quantity = Flt

type Wage = Flt
type Rental = Flt
type Capital = Flt
type Labor = Flt

type DemandCurve = Curve
type SupplyCurve = Curve
type MarginalCostFunction = Curve



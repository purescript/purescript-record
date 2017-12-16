module Data.Record.Homogeneous
  ( mapValues
  , class MapValues
  , mapValuesImpl
  ) where

import Data.Record (get, insert)
import Data.Symbol (class IsSymbol, SProxy(..))
import Type.Row (class RowLacks, class RowToList, Cons, Nil, RLProxy(..))
import Type.Row.Homogeneous (class Homogeneous, class HomogeneousRowList)

mapValues
  :: forall r t r' t' fields
   . RowToList r fields
  => MapValues fields r r' t t'
  => (t -> t')
  -> Record r
  -> Record r'
mapValues f r = mapValuesImpl (RLProxy :: RLProxy fields) f r

class ( Homogeneous row fieldType
      , HomogeneousRowList rl fieldType
      , Homogeneous row' fieldType'
      )
   <= MapValues rl row row' fieldType fieldType'
    | rl -> row'
    , row' -> fieldType'
    , row -> fieldType
  where 
    mapValuesImpl
      :: RLProxy rl
      -> (fieldType -> fieldType')
      -> Record row
      -> Record row'

instance mapValuesCons ::
  ( MapValues tail row tailRow' fieldType fieldType'
  , Homogeneous row fieldType
  , Homogeneous row' fieldType'
  , IsSymbol name
  , RowCons name fieldType tailRow row
  , RowLacks name tailRow'
  , RowCons name fieldType' tailRow' row'
  ) => MapValues (Cons name fieldType tail) row row' fieldType fieldType'
  where
    mapValuesImpl _ f record = insert nameP value rest
      where
        nameP = SProxy :: SProxy name
        value = f (get nameP record)
        rest = mapValuesImpl (RLProxy :: RLProxy tail) f record

instance mapValuesNil
  :: Homogeneous row fieldType
  => MapValues Nil row () fieldType fieldType'
  where
    mapValuesImpl _ _ _ = {}

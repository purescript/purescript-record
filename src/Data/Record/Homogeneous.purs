module Data.Record.Homogeneous
  ( mapValues
  , class MapValues
  , mapValuesImpl
  ) where

import Type.Row (class RowToList, Nil, RLProxy(..))
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
    , row -> rl fieldType
    , row'-> fieldType'
  where 
    mapValuesImpl
      :: RLProxy rl
      -> (fieldType -> fieldType')
      -> Record row
      -> Record row'

instance mapValuesNil
  :: Homogeneous row fieldType => MapValues Nil row () fieldType fieldType' where
  mapValuesImpl _ _ _ = {}
  

{-
mapRowListValues
  ::
   . HomogeneousRowList 
-}

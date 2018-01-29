module Data.Record.Homogeneous
  ( mapValues
  , class MapValues
  , mapValuesImpl

  , mapWithIndex
  , class MapWithIndex
  , mapWithIndexImpl
  ) where

import Control.Category (id, (<<<))
import Data.Record (get)
import Data.Record.Builder (Builder, build, insert)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Type.Row (class RowLacks, class RowToList, Cons, Nil, RLProxy(..))
import Type.Row.Homogeneous (class Homogeneous, class HomogeneousRowList)

mapValues
  :: forall r t r' t' fields
   . RowToList r fields
  => MapValues fields r r' t t'
  => (t -> t')
  -> Record r
  -> Record r'
mapValues f r = build (mapValuesImpl (RLProxy :: RLProxy fields) f r) {}

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
      -> Builder {} (Record row')

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
    mapValuesImpl _ f record = insert nameP value <<< rest
      where
        nameP = SProxy :: SProxy name
        value = f (get nameP record)
        rest = mapValuesImpl (RLProxy :: RLProxy tail) f record

instance mapValuesNil
  :: Homogeneous row fieldType
  => MapValues Nil row () fieldType fieldType'
  where
    mapValuesImpl _ _ _ = id

mapWithIndex
  :: forall r t r' t' fields
   . RowToList r fields
  => MapWithIndex fields r r' t t'
  => (String -> t -> t')
  -> Record r
  -> Record r'
mapWithIndex f r = build (mapWithIndexImpl (RLProxy :: RLProxy fields) f r) {}

class ( Homogeneous row fieldType
      , HomogeneousRowList rl fieldType
      , Homogeneous row' fieldType'
      )
   <= MapWithIndex rl row row' fieldType fieldType'
    | rl -> row'
    , row' -> fieldType'
    , row -> fieldType
  where 
    mapWithIndexImpl
      :: RLProxy rl
      -> (String -> fieldType -> fieldType')
      -> Record row
      -> Builder {} (Record row')

instance mapWithIndexCons ::
  ( MapWithIndex tail row tailRow' fieldType fieldType'
  , Homogeneous row fieldType
  , Homogeneous row' fieldType'
  , IsSymbol name
  , RowCons name fieldType tailRow row
  , RowLacks name tailRow'
  , RowCons name fieldType' tailRow' row'
  ) => MapWithIndex (Cons name fieldType tail) row row' fieldType fieldType'
  where
    mapWithIndexImpl _ f record = insert nameP value <<< rest
      where
        nameP = SProxy :: SProxy name
        rest = mapWithIndexImpl (RLProxy :: RLProxy tail) f record
        value = f (reflectSymbol nameP) (get nameP record)

instance mapWithIndexNil
  :: Homogeneous row fieldType
  => MapWithIndex Nil row () fieldType fieldType'
  where
    mapWithIndexImpl _ _ _ = id

module Data.Record.RowLabels
  ( class RowLabels
  , labels
  ) where

import Data.Array (cons)
import Type.Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Type.Row (class ListToRow, class RowToList, Cons, Nil, RProxy(..), kind RowList)

class RowLabels (l :: RowList) where
  labels
    :: forall r
     . RowToList r l
    => ListToRow l r
    => RProxy r
    -> Array String

instance getLabelsNil :: RowLabels Nil where
  labels _ = []

instance getLabelsCons
  :: ( IsSymbol name
     , RowLabels tail
     , ListToRow tail row
     , RowToList row tail
     )
  => RowLabels (Cons name ty tail) where
  labels _ = cons (reflectSymbol (SProxy :: SProxy name)) (labels (RProxy :: RProxy row))

module Game.Data.Cycled where

import Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.List.NonEmpty as NE

data Cycled a = Cycled (NonEmpty a) [a]
  deriving (Eq, Ord, Show)

mkCycled :: NonEmpty a -> Cycled a
mkCycled xs = Cycled xs []

next :: Cycled a -> (a, Cycled a)
next (Cycled (x :| xs) bt) = (x,) $ case xs of
  (y:ys) -> Cycled (y :| ys) (x : bt)
  [] -> Cycled (NE.reverse (x NE.:| bt)) []

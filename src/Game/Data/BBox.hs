module Game.Data.BBox where

import Game.Prelude
import SDL.Vect

data BBoxF f
  = BBox (V2 f) (V2 f)
  | BCircle (V2 f) f
  deriving (Eq, Ord, Show)

type BBox = BBoxF Field

mkBoxBB :: World -> World -> BBox
mkBoxBB = flip BBox

mkCircleBB :: Field -> World -> BBox
mkCircleBB = flip BCircle

isWithin :: BBox -> World -> Bool
isWithin (BBox c s) p = c <= p && p <= c + s
isWithin (BCircle c r) p = quadrance (c - p) <= r * r

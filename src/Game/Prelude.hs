module Game.Prelude (
  module Game.Prelude,
  bimap, first, second, void,
  V2(V2), Word32
) where

import Data.Bifunctor (bimap, first, second, Bifunctor)
import Control.Monad (void)
import SDL.Vect
import Data.Word
import Data.Int

($$) = ($)
infixr 6 $$

(<<$>>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<<$>>) = fmap . fmap
infixl 4 <<$>>

type Field = Double
type World = V2 Field
type Screen = Point V2 Int32

-- | In do blocks, `let` is particular about the indentation of nested do blocks
-- Rewrite to binds to avoid double indenting
is :: Applicative m => a -> m a
is = pure

both :: Bifunctor t => (a -> b) -> t a a -> t b b
both f = bimap f f

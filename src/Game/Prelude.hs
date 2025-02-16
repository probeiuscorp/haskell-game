module Game.Prelude (
  module Game.Prelude,
  bimap, first, second, void
) where

import Data.Bifunctor (bimap, first, second, Bifunctor)
import Control.Monad (void)

-- | In do blocks, `let` is particular about the indentation of nested do blocks
-- Rewrite to binds to avoid double indenting
is :: Applicative m => a -> m a
is = pure

both :: Bifunctor t => (a -> b) -> t a a -> t b b
both f = bimap f f

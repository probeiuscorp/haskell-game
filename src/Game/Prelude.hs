module Game.Prelude where

-- | In do blocks, `let` is particular about the indentation of nested do blocks
-- Rewrite to binds to avoid double indenting
is :: Applicative m => a -> m a
is = pure

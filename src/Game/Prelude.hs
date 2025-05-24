module Game.Prelude (
  module Game.Prelude,
  ($>), bimap, first, second, void,
  V2(V2), Word32
) where

import Data.Functor (($>), (<&>))
import Data.Bifunctor (bimap, first, second, Bifunctor)
import Control.Monad (void)
import Reactive.Banana.Combinators
import SDL.Vect
import Data.Word
import Data.Int
import qualified Debug.Trace as Debug
import Data.Maybe (fromMaybe)
import Reactive.Banana.Frameworks (execute, MomentIO)

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

pairA :: Applicative f => f a -> f b -> f (a, b)
pairA = liftA2 (,)

pairBE :: Behavior a -> Event b -> Event (a, b)
pairBE b e = ((,) <$> b) <@> e

both :: Bifunctor t => (a -> b) -> t a a -> t b b
both f = bimap f f

(??) :: Maybe a -> a -> a
(??) = flip fromMaybe
infixl 6 ??

-- | Having to import trace then remove the import later is really painful
debug :: String -> a -> a
debug = Debug.trace
debugShow :: Show a => a -> b -> b
debugShow = Debug.traceShow
debugShowId :: Show a => a -> a
debugShowId = Debug.traceShowId

-- | Merge event streams, but if both happen at the same time, forget both events.
-- Useful when the events divide each other, but the composition is complicated.
annihilateMerge :: Event a -> Event a -> Event a
annihilateMerge e1 e2 = filterJust $ mergeWith Just Just (const . const $ Nothing) e1 e2

-- | Drop events which satisfy predicate on previous event
changesE :: MonadMoment m => (a -> a -> Bool) -> Event a -> m (Event a)
changesE p e = do
  eDistinct <- accumE Nothing $ e <&> \x -> \case
    Nothing -> Just x
    Just y -> if p x y then Nothing else Just x
  pure $ filterJust eDistinct

-- | Rebuild accumB's on new data from event
switchM :: a -> Event a -> (a -> MomentIO (Behavior b)) -> MomentIO (Behavior b)
switchM initial e mk = do
  eb <- execute $ mk <$> e
  bInitial <- mk initial
  switchB bInitial eb

true :: a -> b -> a
true = const
false :: a -> b -> b
false = const id

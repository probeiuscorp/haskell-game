module Game.Prelude (
  module Game.Prelude,
  module Reactive.Banana,
  ($>), bimap, first, second, void,
  V2(V2), Word32
) where

import Data.Functor (($>), (<&>))
import Data.Bifunctor (bimap, first, second, Bifunctor)
import Control.Monad (void)
import Reactive.Banana
import qualified SDL
import SDL.Vect
import Data.Word
import qualified Debug.Trace as Debug
import Data.Maybe (fromMaybe)
import Reactive.Banana.Frameworks (execute, MomentIO)
import Foreign.C (CInt)

($:) :: (a -> b) -> a -> b
($:) = ($)
infixr 6 $:

($$) :: Functor f => (a -> b) -> f a -> f b
($$) = fmap
($$$) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
($$$) = fmap . fmap
($$$$) :: (Functor f, Functor g, Functor h) => (a -> b) -> f (g (h a)) -> f (g (h b))
($$$$) = fmap . ($$$)
infixl 4 $$, $$$, $$$$

(#) :: a -> (a -> b) -> b
(#) = flip ($)
infixl 1 #

(##) :: Functor f => f a -> (a -> b) -> f b
(##) = flip ($$)
(###) :: (Functor f, Functor g) => f (g a) -> (a -> b) -> f (g b)
(###) = flip ($$$)
(####) :: (Functor f, Functor g, Functor h) => f (g (h a)) -> (a -> b) -> f (g (h b))
(####) = flip ($$$$)
infixl 4 ##, ###, ####

(@@) :: Behavior (a -> b) -> Event a -> Event b
(@@) = (<@>)
infixl 4 @@

(<+>) :: (Applicative f, Semigroup a) => f a -> f a -> f a
(<+>) = liftA2 (<>)
infixr 6 <+>

(.:) :: (a -> b) -> (c -> d -> a) -> (c -> d -> b)
(.:) = (.) . (.)
(.:.) :: (a -> b) -> (c -> d -> e -> a) -> (c -> d -> e -> b)
(.:.) = (.) . (.) . (.)
(.::) :: (a -> b) -> (c -> d -> e -> f -> a) -> (c -> d -> e -> f -> b)
(.::) = (.) . (.) . (.) . (.)
infixr 9 .: , .:. , .::

type Field = Double
type World = V2 Field
type ScreenField = CInt
type Screen = Point V2 ScreenField

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

resampleM :: Event () -> MomentIO a -> MomentIO (Behavior a)
resampleM e m = do
  initial <- m
  em <- execute $ m <$ e
  stepper initial em

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

withValue stateVar v (m :: IO ()) = do
  old <- SDL.get stateVar
  stateVar SDL.$= v
  m
  stateVar SDL.$= old

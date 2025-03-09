{-# LANGUAGE DeriveFunctor #-}

module Game.Data.Queue where

data Queue a = Queue [a] [a]
  deriving (Eq, Ord, Show, Functor)

enqueue :: a -> Queue a -> Queue a
enqueue x (Queue top back) = Queue top $ x : back

dequeue :: Queue a -> Maybe (a, Queue a)
dequeue (Queue (x:xs) back) = Just (x, Queue xs back)
dequeue (Queue [] back@(_:_)) = Just $ let (x:xs) = reverse back in (x, Queue xs [])
dequeue (Queue [] []) = Nothing

singleton :: a -> Queue a
singleton x = Queue [x] []

instance Semigroup (Queue a) where
  Queue a b <> Queue c d = Queue (a <> reverse b <> c) d
instance Monoid (Queue a) where
  mempty = Queue [] []
instance Foldable Queue where
  foldr f b t = case dequeue t of
    Just (x, tr) -> f x $ foldr f b tr
    Nothing -> b

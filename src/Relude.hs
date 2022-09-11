module Relude
  ( module Prelude,
    foldMapM,
    mapLeft,
    first,
    second,
  )
where

import Prelude

import Data.Bifunctor (first, second)

infixl 9 >>>

(>>>) :: (a -> b) -> (b -> c) -> (a -> c)
f >>> g = g . f
{-# INLINE (>>>) #-}

foldMapM :: (Monad m, Monoid b) => (a -> m b) -> [a] -> m b
foldMapM f = mapM f >>> fmap mconcat

mapLeft :: (e -> e') -> Either e a -> Either e' a
mapLeft f = either (f >>> Left) Right

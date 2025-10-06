{-# LANGUAGE ScopedTypeVariables #-}
{- |
  These functions an re-exported from 'Flipstone.Prelude' as 'Foldable' and other
  related functions are already part of 'Prelude' in @base@.
-}
module Flipstone.Extras.Foldable
  ( foldMapA
  , foldMapA1
  ) where

import Data.Coerce (coerce)
import Data.Monoid (Ap(..))
import Data.Foldable1 (Foldable1, foldMap1)
import Prelude (Applicative, Monoid, Semigroup, Foldable, foldMap)

{- | Execute a 'foldMap' within an 'Applicative', allowing side effects while producing elements to combine -}
foldMapA :: forall f t a b. (Monoid b, Applicative f, Foldable t) => (a -> f b) -> t a -> f b
foldMapA = coerce (foldMap :: (a -> Ap f b) -> t a -> Ap f b)

{- | Execute a 'foldMap1' within an 'Applicative', allowing side effects while producing elements to combine -}
foldMapA1 :: forall f t a b. (Semigroup b, Applicative f, Foldable1 t) => (a -> f b) -> t a -> f b
foldMapA1 = coerce (foldMap1 :: (a -> Ap f b) -> t a -> Ap f b)

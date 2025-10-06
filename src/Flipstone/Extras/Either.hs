{- |
  These functions an re-exported from 'Flipstone.Prelude' as 'Either' and other
  related functions are already part of 'Prelude' in @base@.
-}
module Flipstone.Extras.Either
  ( mapBoth
  , mapLeft
  , mapRight
  ) where

import Data.Bifunctor (bimap, first, second)
import Prelude (Either)

{- | A type-restricted alias of 'Bifunctor' 'bimap' for clarity and directness -}
mapBoth :: (a -> c) -> (b -> d) -> Either a b -> Either c d
mapBoth = bimap

{- | A type-restricted alias of 'Bifunctor' 'first' for clarity and directness -}
mapLeft :: (a -> c) -> Either a b -> Either c b
mapLeft = first

{- | A type-restricted alias of 'Bifunctor' 'second' for clarity and directness -}
mapRight :: (b -> c) -> Either a b -> Either a c
mapRight = second

{- |
  These functions an re-exported from 'Flipstone.Prelude' as 'Functor' and other
  related functions are already part of 'Prelude' in @base@.
-}
module Flipstone.Extras.Functor
  ( ffmap
  ) where

import Prelude (Functor, fmap, (.))

{- | Convenience function for fmapping through two layed functors at once | -}
ffmap :: (Functor f, Functor g)
      => (a -> b)
      -> f (g a)
      -> f (g b)
ffmap = fmap . fmap


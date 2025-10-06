{- |
  This module provides names for some common arrow operators. It is not,
  however, included in the 'Flipstone.Prelude' module because 'Control.Arrow'
  is not part of 'Prelude'.
-}
module Flipstone.Extras.Arrow
  ( fanout
  , splitBoth
  , fanin
  , splitEither
  ) where

import Control.Arrow (Arrow((&&&), (***)), ArrowChoice((|||), (+++)))
import Data.Either (Either)

{- | The name for the @&&&@ 'Arrow' operator -}
fanout :: Arrow a => a b c -> a b d -> a b (c, d)
fanout = (&&&)

{- | The name for the @***@ 'Arrow' operator -}
splitBoth :: Arrow a => a b c -> a d e -> a (b, d) (c, e)
splitBoth = (***)

{- | The name for the @|||@ 'ArrowChoice' operator -}
fanin :: ArrowChoice a => a b d -> a c d -> a (Either b c) d
fanin = (|||)

{- | The name for the @+++@ 'ArrowChoice' operator -}
splitEither :: ArrowChoice a => a b c -> a d e -> a (Either b d) (Either c e)
splitEither = (+++)

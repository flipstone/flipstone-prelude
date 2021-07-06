module Flipstone.Prelude
 (
 -- Concrete types
   Bool(..)
 , Ordering(..)
 , Char
 , Int
 , Integer
 , Rational
 , Int8
 , Int16
 , Int32
 , Int64
 , Ratio
 , Word8
 , Word16
 , Word32
 , Word64

 -- Parameterized types
 -- Maybe and related functions
 , Maybe(..)
 , maybe
 -- Either and related
 , Either(..)
 , either

 -- IO and related
 , IO
 , liftIO

 -- Typeclasses

 -- Comparison typeclasses and functions
 , Eq(..)
 , Ord(..)

 -- Monad related-ish classes and functions
 , Functor(..)
 , Applicative(..)
 , Semigroup(..)
 , Monoid(mempty, mconcat) -- `mappend` is redundant and not part of the minimal complete definition
 , Monad((>>=), (>>)) -- `return` is redundant and not part of the minimal complete definition
 , MonadFail(..)
 , Arrow(..)
 , MonadPlus(..)
 , Alternative(..)
 , liftA
 , liftA3
 , guard
 , join
 , void
 , when

 -- Fold and traversal typeclasses and functions
 , Traversable(..)
 , Foldable(foldMap, foldr, foldr1, elem, maximum, sum, product)
 , null
 , length
 , and
 , or
 , any
 , all
 , concat
 , concatMap
 , asum
 , foldl'
 , sequenceA_
 , traverse_

 -- Enumeration typeclasses
 , Enum(..)
 , Bounded(..)

 -- Numeric Typeclasses and functions
 , Num(..)
 , Real(..)
 , Integral(..)
 , Fractional(..)
 , RealFrac(..)
 , subtract
 , even
 , odd
 , gcd
 , lcm
 , fromIntegral
 , realToFrac
 ) where

import Prelude ( Bool(..)
               , Ordering(..)
               , Char
               , Int
               , Integer
               , Rational
               , Maybe(..)
               , maybe
               , Either(..)
               , either
               , IO
               , Eq(..)
               , Ord(..)
               , Semigroup(..)
               , Monoid(mempty, mconcat) -- | `mappend` is redundant and not part of the minimal complete definition
               , Functor(..)
               , Applicative(..)
               , Monad(..)
               , MonadFail(..)
               , Traversable(..)
               , Foldable(foldMap, foldr, foldr1, elem, maximum, sum, product) -- | 'foldl' considered dangerous, use 'foldl\'' instead.
               , null
               , length
               , and
               , or
               , any
               , all
               , concat
               , concatMap
               , Enum(..)
               , Bounded(..)
               , Num(..)
               , Real(..)
               , Integral(..)
               , Fractional(..)
               , RealFrac(..)
               , subtract
               , even
               , odd
               , gcd
               , lcm
               , fromIntegral
               , realToFrac
               )

import Control.Applicative (Alternative(..), liftA, liftA3)
import Control.Arrow (Arrow(..))
import Control.Monad (MonadPlus(..), guard, join, void, when)
import Control.Monad.IO.Class (liftIO)
import Data.Foldable (asum, foldl', sequenceA_, traverse_)
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Ratio (Ratio)
import Data.Word (Word8, Word16, Word32, Word64)

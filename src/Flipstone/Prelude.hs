{-# LANGUAGE ScopedTypeVariables #-}
module Flipstone.Prelude
 (
 -- Concrete types and related functions
   Bool(True, False)
 , (&&)
 , (||)
 , not
 , otherwise

 , Ordering(LT, EQ, GT)

 -- Character types
 , Char
 , String

 -- Numeric types
 , Int
 , Integer
 , Rational
 , Int8
 , Int16
 , Int32
 , Int64
 , Ratio
 , Word
 , Word8
 , Word16
 , Word32
 , Word64

 -- Types that we only re-export the Type not associated functions
 , NonEmpty

 -- Maybe and related functions
 , Maybe(Just, Nothing)
 , maybe

 -- Either and related
 , Either(Left, Right)
 , either
 , lefts
 , rights
 , isLeft
 , isRight
 , fromLeft
 , fromRight
 , mapBoth
 , mapLeft
 , mapRight

 -- IO and related
 , IO
 , liftIO
 , MonadIO

 -- Typeclasses

 -- Comparison typeclasses and functions
 , Eq((==), (/=))
 , Ord(compare, (<), (<=), (>), (>=), max, min)

 -- Monad related-ish classes and functions
 , Functor(fmap, (<$))
 , Applicative(pure, (<*>), liftA2, (*>), (<*))
 , Semigroup((<>), sconcat, stimes)
 , Monoid(mempty, mconcat) -- `mappend` is redundant and not part of the minimal complete definition
 , Monad((>>=), (>>)) -- `return` is redundant and not part of the minimal complete definition
 , MonadFail(fail)
 , ffmap
 , liftA3
 , join
 , void
 , when
 , (<$>)
 , (=<<)

 -- Fold and traversal typeclasses and functions
 , Traversable(traverse, sequenceA)
 , Foldable(
   fold, foldMap, foldMap', foldr, foldr', foldl', toList, null, length, elem, sum, product
 )
 -- ^ `foldl` is considered dangerous, also omitted are the functions which will throw, `foldr1` and `foldl1`
 , and
 , or
 , any
 , all
 , concat
 , concatMap
 , asum
 , sequenceA_
 , traverse_
 , find

 -- Enumeration typeclasses
 , Enum(succ, pred, toEnum, fromEnum, enumFrom, enumFromThen, enumFromTo, enumFromThenTo)
 , Bounded(minBound, maxBound)
 , boundedEnumFrom
 , boundedEnumFromThen

 -- Numeric Typeclasses and functions
 , Num((+), (-), (*), negate, abs, signum, fromInteger)
 , Real(toRational)
 , Integral(quot, rem, div, mod, quotRem, divMod, toInteger)
 , Fractional((/), recip, fromRational)
 , RealFrac(properFraction, truncate, round, ceiling, floor)
 , subtract
 , fromIntegral
 , realToFrac

 -- Function combinators
 , id
 , const
 , (.)
 , flip
 , ($)

-- Errors and Debugging
 , trace
 , traceIO
 , traceShowM
 , traceShowId
 , traceStack
 , undefined

-- Tuples
 , curry
 , fst
 , snd
 , swap
 , uncurry

-- Foldable1
 , Foldable1
    ( fold1
    , foldMap1
    , foldMap1'
    , toNonEmpty
    , maximum
    , minimum
    , head
    , last
    , foldrMap1
    , foldlMap1'
    --, foldlMap1
    , foldrMap1'
    )
 , foldr1
 , foldr1'
 --, foldl1
 , foldl1'
 , intercalate1
 , foldrM1
 , foldlM1
 , foldrMapM1
 , foldlMapM1
 , maximumBy
 , minimumBy
 , foldMapA
 , foldMapA1
 ) where

import Control.Applicative( Applicative(pure, (<*>), liftA2, (*>), (<*)), liftA3 )

import Control.Monad ( join, Monad((>>), (>>=)), when, (=<<))
import Control.Monad.Fail ( MonadFail(fail) )
import Control.Monad.IO.Class ( MonadIO, liftIO )
import Data.Bool (Bool(True, False), (&&), (||), not, otherwise)
import Data.Char (Char)
import Data.Either ( Either(Left, Right), either, lefts, rights, isLeft, isRight )
import Data.Either.Combinators (fromLeft, fromRight, mapBoth, mapLeft, mapRight )
import Data.Eq ( Eq((==), (/=)) )
import Data.Foldable ( Foldable(
                                 fold, foldMap, foldMap', foldr, foldr', foldl', toList, null, length, elem, maximum, sum, product
                               )
                     , and
                     , or
                     , any
                     , all
                     , concat
                     , concatMap
                     , asum
                     , sequenceA_
                     , traverse_
                     , find
                     )
import Data.Foldable1
  ( Foldable1
    (fold1
    , foldMap1
    , foldMap1'
    , toNonEmpty
    , maximum
    , minimum
    , head
    , last
    , foldrMap1
    , foldlMap1'
    --, foldlMap1
    , foldrMap1'
    )
  , foldr1
  , foldr1'
  --, foldl1
  , foldl1'
  , intercalate1
  , foldrM1
  , foldlM1
  , foldrMapM1
  , foldlMapM1
  , maximumBy
  , minimumBy
  )

import Data.Coerce (coerce)
import Data.Function (id, const, (.), flip, ($))
import Data.Functor (Functor(fmap, (<$)), (<$>), void)
import Data.Int (Int, Int8, Int16, Int32, Int64)
import Data.Maybe ( Maybe(Just, Nothing), maybe )
import Data.Monoid ( Ap(..), Monoid(mconcat, mempty) )
import Data.List.NonEmpty (NonEmpty)
import Data.Ord ( Ord(compare, (<), (<=), (>), (>=), max, min), Ordering(LT, EQ, GT) )
import Data.Ratio ( Ratio, Rational )
import Data.Semigroup ( Semigroup((<>), sconcat, stimes))
import Data.String ( String )
import Data.Traversable ( Traversable(traverse, sequenceA) )
import Data.Tuple (curry, fst, snd, swap, uncurry)
import Data.Word ( Word, Word8, Word16, Word32, Word64 )
import Flipstone.Debug ( trace , traceIO , traceShowId , traceShowM , traceStack , undefined)
import GHC.Enum ( Bounded(minBound, maxBound)
                , Enum(succ, pred, toEnum, fromEnum, enumFrom, enumFromThen, enumFromTo, enumFromThenTo)
                , boundedEnumFrom
                , boundedEnumFromThen
                )
import GHC.IO ( IO )
import GHC.Num ( Num((+), (-), (*), negate, abs, signum, fromInteger), Integer, subtract )
import GHC.Real( fromIntegral
               , realToFrac
               , Fractional((/), recip, fromRational)
               , Integral(quot, rem, div, mod, quotRem, divMod, toInteger)
               , Real(toRational)
               , RealFrac(properFraction, truncate, round, ceiling, floor)
               )

ffmap :: (Functor f, Functor g)
      => (a -> b)
      -> f (g a)
      -> f (g b)
ffmap = fmap . fmap

foldMapA :: forall f t a b. (Monoid b, Applicative f, Foldable t) => (a -> f b) -> t a -> f b
foldMapA = coerce (foldMap :: (a -> Ap f b) -> t a -> Ap f b)

foldMapA1 :: forall f t a b. (Semigroup b, Applicative f, Foldable1 t) => (a -> f b) -> t a -> f b
foldMapA1 = coerce (foldMap1 :: (a -> Ap f b) -> t a -> Ap f b)

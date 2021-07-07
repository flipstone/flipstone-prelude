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

 -- Parameterized types

 -- Maybe and related functions
 , Maybe(Just, Nothing)
 , maybe

 -- Either and related
 , Either(Left, Right)
 , either

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
 , liftA3
 , join
 , void
 , when

 -- Fold and traversal typeclasses and functions
 , Traversable(traverse, sequenceA)
 , Foldable(fold, foldMap, foldMap', foldr, foldr', foldl', toList, null, length, elem, maximum, sum, product)
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
 , maximumBy
 , minimumBy
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

 ) where

import Control.Applicative( Applicative(pure, (<*>), liftA2, (*>), (<*)), liftA3 )
import Control.Monad ( join, Monad((>>), (>>=)), MonadFail(fail), when)
import Control.Monad.IO.Class ( MonadIO, liftIO )
import Data.Bool (Bool(True, False), (&&), (||), not, otherwise)
import Data.Char (Char)
import Data.Either ( Either(Left, Right), either )
import Data.Eq ( Eq((==), (/=)) )
import Data.Foldable ( Foldable(fold, foldMap, foldMap', foldr, foldr', foldl', toList, null, length, elem, maximum, sum, product) -- 'foldl' considered dangerous, use 'foldl\'' instead.
                     , and
                     , or
                     , any
                     , all
                     , concat
                     , concatMap
                     , asum
                     , sequenceA_
                     , traverse_
                     , maximumBy
                     , minimumBy
                     , find
                     )
import Data.Function (id, const, (.), flip, ($))
import Data.Functor (Functor(fmap, (<$)), void)
import Data.Int (Int, Int8, Int16, Int32, Int64)
import Data.Maybe ( Maybe(Just, Nothing), maybe )
import Data.Monoid ( Monoid(mconcat, mempty) )
import Data.Ord ( Ord(compare, (<), (<=), (>), (>=), max, min), Ordering(LT, EQ, GT) )
import Data.Ratio ( Ratio, Rational )
import Data.Semigroup ( Semigroup((<>), sconcat, stimes))
import Data.Traversable ( Traversable(traverse, sequenceA) )
import Data.Word ( Word, Word8, Word16, Word32, Word64 )
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

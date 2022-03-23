{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Flipstone.Debug
  ( trace
  , traceIO
  , traceShowId
  , traceShowM
  , traceStack
  , undefined
  ) where

#ifdef DEV
-- This exposes the true versions of the debug functions for use in development
-- environments. They will only compile if there is a `-DDEV` compiler flag
-- present.
import Debug.Trace (trace, traceIO, traceShowId, traceShowM, traceStack)
import GHC.Err (undefined)

#else
-- This exposes bunk versions of the debug functions that will always fail to
-- compile when used.
import Data.Kind (Constraint, Type)
import qualified GHC.Err as Err
import Type.Errors (TypeError, ErrorMessage(Text, (:$$:), (:<>:)))

class DevFlagNotSetError (msg :: ErrorMessage)
instance TypeError msg => DevFlagNotSetError msg

type DevFlagNotSetMessage =
  'Text "Debugging functions are only permitted in development "         ':$$:
  'Text "environments. In order to use this function, you must set the " ':$$:
  'Text "DEV flag when loading the REPL. This flag must never be used "  ':$$:
  'Text "in any live environment. "

trace :: DevFlagNotSetError DevFlagNotSetMessage => a
trace = Err.error "unreachable"

traceIO :: DevFlagNotSetError DevFlagNotSetMessage => a
traceIO = Err.error "unreachable"

traceShowId :: DevFlagNotSetError DevFlagNotSetMessage => a
traceShowId = Err.error "unreachable"

traceShowM :: DevFlagNotSetError DevFlagNotSetMessage => a
traceShowM = Err.error "unreachable"

traceStack :: DevFlagNotSetError DevFlagNotSetMessage => a
traceStack = Err.error "unreachable"

undefined :: DevFlagNotSetError DevFlagNotSetMessage => a
undefined = Err.error "unreachable"

#endif

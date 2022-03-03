{-# LANGUAGE CPP #-}
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
import qualified GHC.Err as Err

data DevFlagNotSet

{-# WARNING trace, traceIO, traceShowId, traceShowM, traceStack, undefined
    "Debug functions may only be used in development."
  #-}

trace :: DevFlagNotSet
trace = Err.undefined

traceIO :: DevFlagNotSet
traceIO = Err.undefined

traceShowId :: DevFlagNotSet
traceShowId = Err.undefined

traceShowM :: DevFlagNotSet
traceShowM = Err.undefined

traceStack :: DevFlagNotSet
traceStack = Err.undefined

undefined :: DevFlagNotSet
undefined = Err.undefined

#endif

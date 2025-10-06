{- |
  This module contains functions that are useful during debugging and
  development, but that should rarely (if every) be present in production
  code. The versions exported by this module produce a warning, which
  is routinely an error because we commonly use '-Werror'. This can
  be overridden on the command line during development to use these
  functions .

  E.G. @stack test --ghc-options=-Wwarn=x-flipstone-debug
-}
module Flipstone.Debug
  ( trace
  , traceIO
  , traceShowId
  , traceShowM
  , traceStack
  , undefined
  , error
  ) where

import Control.Applicative (Applicative)
import Data.String (String)
import qualified Debug.Trace as Trace
import qualified GHC.Err as Err
import System.IO (IO)
import Text.Show (Show)

{-# WARNING in "x-flipstone-debug" trace "Debugging functions are disabled by default. Pass --ghc-options=-Wwarn=x-flipstone-debug on the command line (e.g. to a script that pass arguments to stack) to temporarily enable them." #-}
trace :: String -> a -> a
trace = Trace.trace

{-# WARNING in "x-flipstone-debug" traceIO "Debugging functions are disabled by default. Pass --ghc-options=-Wwarn=x-flipstone-debug on the command line (e.g. to a script that pass arguments to stack) to temporarily enable them." #-}
traceIO :: String -> IO ()
traceIO = Trace.traceIO

{-# WARNING in "x-flipstone-debug" traceShowId "Debugging functions are disabled by default. Pass --ghc-options=-Wwarn=x-flipstone-debug on the command line (e.g. to a script that pass arguments to stack) to temporarily enable them." #-}
traceShowId :: Show a => a -> a
traceShowId = Trace.traceShowId

{-# WARNING in "x-flipstone-debug" traceShowM "Debugging functions are disabled by default. Pass --ghc-options=-Wwarn=x-flipstone-debug on the command line (e.g. to a script that pass arguments to stack) to temporarily enable them." #-}
traceShowM :: (Show a, Applicative f) => a -> f ()
traceShowM = Trace.traceShowM

{-# WARNING in "x-flipstone-debug" traceStack "Debugging functions are disabled by default. Pass --ghc-options=-Wwarn=x-flipstone-debug on the command line (e.g. to a script that pass arguments to stack) to temporarily enable them." #-}
traceStack :: String -> a -> a
traceStack = Trace.traceStack

{-# WARNING in "x-flipstone-debug" undefined "Debugging functions are disabled by default. Pass --ghc-options=-Wwarn=x-flipstone-debug on the command line (e.g. to a script that pass arguments to stack) to temporarily enable them." #-}
undefined :: a
undefined = Err.undefined

{-# WARNING in "x-flipstone-debug" error "Debugging functions are disabled by default. Pass --ghc-options=-Wwarn=x-flipstone-debug on the command line (e.g. to a script that pass arguments to stack) to temporarily enable them." #-}
error :: String -> a
error = Err.error

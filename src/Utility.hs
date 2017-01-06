module Utility where

import System.IO
import Control.Concurrent
import Control.Exception


sendSafe :: Handle -> String -> IO ()
sendSafe hand str = finally (hPutStrLn hand str) (return ())

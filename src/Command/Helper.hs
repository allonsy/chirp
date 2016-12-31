module Command.Helper where

import qualified Server as S
import qualified User as U
import qualified Reply as R
import qualified Message as M
import System.IO
import Control.Concurrent.MVar
import Control.Exception


sendNeedMoreParams :: MVar U.User -> S.Server -> String -> IO ()
sendNeedMoreParams userVar serv cmd = do
  use <- takeMVar userVar
  let pref = M.ServerName (S.hostname serv)
  let rep = R.Reply pref 461 [U.nickname use, cmd] "Not enough parameters"
  R.sendReply (U.handle use) rep
  putMVar userVar use

sendSafe :: Handle -> String -> IO ()
sendSafe hand str = finally (hPutStrLn hand str) (return ())

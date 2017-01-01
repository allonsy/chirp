module Command.User where

import qualified User as U
import qualified Reply as R
import qualified Server as S
import qualified Message as M
import Control.Concurrent.MVar


checkUser :: S.Server -> MVar U.User -> IO ()
checkUser serv useVar = do
  use <- takeMVar useVar
  let pref = M.ServerName (S.hostname serv)
  let rep = R.Reply pref 462 [U.nickname use] "Unauthorized command (already registered)"
  R.sendReply (U.handle use) rep
  putMVar useVar use

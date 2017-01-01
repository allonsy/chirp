module Dispatch where

import qualified Server as S
import qualified User as U
import qualified Message as M
import qualified Reply as R
import System.IO
import Control.Concurrent.MVar
import Data.Char
import Command.Nick
import Command.User

dispatch :: S.Server -> MVar U.User -> Handle -> IO ()
dispatch serv userVar hand = do
  isHandClosed <- hIsEOF hand
  if isHandClosed then return ()
  else do
    msg <- hGetLine hand
    let cmdEither = M.parseMessage msg
    case cmdEither of
      Left _ -> sendUnknownCmd userVar serv "*" >> dispatch serv userVar hand
      Right cmd -> case map toUpper (M.command cmd) of
        "NICK" -> changeNick serv userVar cmd >> dispatch serv userVar hand
        "USER" -> checkUser serv userVar >> dispatch serv userVar hand
        _ -> sendUnknownCmd userVar serv (M.command cmd) >> dispatch serv userVar hand


sendUnknownCmd :: MVar U.User -> S.Server -> String -> IO ()
sendUnknownCmd userVar serv cmd = do
  use <- takeMVar userVar
  let pref = M.ServerName (S.hostname serv)
  let rep = R.Reply pref 421 [U.nickname use, cmd] "Unknown command"
  R.sendReply (U.handle use) rep
  putMVar userVar use

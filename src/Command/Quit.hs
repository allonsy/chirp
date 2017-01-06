module Command.Quit where

import qualified Server as S
import qualified User as U
import qualified Reply as R
import qualified Message as M
import qualified Data.HashMap.Lazy as HM
import System.IO
import Control.Concurrent.MVar
import Utility

quit :: S.Server -> MVar U.User -> M.Message -> IO ()
quit serv userVar msg = do
  let quitMsg = if M.numParams msg >= 1 then head (M.params msg) else "Client quit"
  user <- takeMVar userVar
  let userNick = U.nickname user
  putMVar userVar user
  userMap <- takeMVar $ S.users serv
  let newUserMap = HM.delete userNick userMap
  putMVar (S.users serv) newUserMap
  nickMap <- takeMVar $ S.nicks serv
  let newNickMap = HM.delete userNick nickMap
  putMVar (S.nicks serv) newNickMap
  newUser <- takeMVar userVar
  let pref = U.genPrefix newUser
  let quitMessage = (M.prefixToString pref) ++ " QUIT :" ++ quitMsg
  sendSafe (U.handle newUser) quitMessage
  putMVar userVar newUser

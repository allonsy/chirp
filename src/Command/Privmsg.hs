module Command.Privmsg where

import qualified Server as S
import qualified Message as M
import qualified User as U
import Control.Concurrent.MVar
import Command.Helper
import qualified Data.HashMap.Lazy as HM

sendPrivMsg :: S.Server -> MVar U.User -> M.Message -> Bool -> IO ()
sendPrivMsg serv userVar msg isNotNotice
  | M.numParams msg < 2  = if isNotNotice then
        sendNeedMoreParams userVar serv (if isNotNotice then "PRIVMSG" else "NOTICE")
      else
        return ()
  | otherwise = do
      let [targetNick, messageContent] = take 2 (M.params msg)
      userMap <- takeMVar (S.users serv)
      case HM.lookup targetNick userMap of
        Nothing -> do
          putMVar (S.users serv) userMap
          if isNotNotice then
            sendNoSuchNick userVar serv targetNick
          else
            return ()
        Just targetUserVar -> do
          putMVar (S.users serv) userMap
          curUser <- takeMVar userVar
          let userPrefix = U.genPrefix curUser
          putMVar userVar curUser
          targetUser <- takeMVar targetUserVar
          let cmdStr = if isNotNotice then " PRIVMSG " else " NOTICE "
          let toSend = (M.prefixToString userPrefix) ++ cmdStr ++ (U.nickname targetUser) ++ " :" ++ messageContent
          sendSafe (U.handle targetUser) toSend
          putMVar targetUserVar targetUser

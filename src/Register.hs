module Register where

import qualified User as U
import qualified Message as M
import qualified Reply as R
import qualified Server as S
import qualified Data.HashMap.Lazy as HM
import System.IO
import Data.Char
import Data.Maybe
import Control.Concurrent.MVar

registerUser :: Handle -> S.Server -> String -> Maybe String -> Maybe String -> Maybe String -> IO (Maybe (MVar U.User))
registerUser hand serv hname nick uname fname = do
  isHandClosed <- hIsEOF hand
  if isHandClosed then return Nothing
  else do
    cmd <- hGetLine hand
    let msgEither = M.parseMessage cmd
    case msgEither of
      Left _ -> sendUnknownCmd hand serv >> registerUser hand serv hname nick uname fname
      Right msg -> do
        case map toUpper (M.command msg) of
          "NICK" -> do
            if M.numParams msg < 1 then sendNoNickGiven hand serv >> registerUser hand serv hname nick uname fname
            else do
              let chosenNick = (M.params msg) !! 0
              if length chosenNick > 9 then sendBadNick hand serv >> registerUser hand serv hname nick uname fname
              else do
                nickMap <- takeMVar (S.nicks serv)
                if HM.member chosenNick nickMap then do
                  sendNickInUse hand serv chosenNick
                  putMVar (S.nicks serv) nickMap
                  registerUser hand serv hname nick uname fname
                else do
                  let newNickMap = HM.insert chosenNick () nickMap
                  putMVar (S.nicks serv) newNickMap
                  if isJust uname then do
                    let newUser = U.User chosenNick (fromJust uname) (fromJust fname) hname hand
                    newUserMVar <- newMVar newUser
                    oldUserMap <- takeMVar (S.users serv)
                    let newUserMap = HM.insert chosenNick newUserMVar oldUserMap
                    putMVar (S.users serv) newUserMap
                    sendWelcome serv newUserMVar
                    return $ Just newUserMVar
                  else do
                    registerUser hand serv hname (Just chosenNick) uname fname
          "USER" -> do
            if M.numParams msg < 4 then do
              sendNeedMoreParams hand serv "USER"
              registerUser hand serv hname nick uname fname
            else do
              let newUsername = (M.params msg) !! 0
              let newFullName = (M.params msg) !! 3
              if isJust nick then do
                let newUser = U.User (fromJust nick) newUsername newFullName hname hand
                newUserMVar <- newMVar newUser
                oldUserMap <- takeMVar (S.users serv)
                let newUserMap = HM.insert (fromJust nick) newUserMVar oldUserMap
                putMVar (S.users serv) newUserMap
                sendWelcome serv newUserMVar
                return $ Just newUserMVar
              else registerUser hand serv hname nick (Just newUsername) (Just newFullName)
          _ -> do
            sendNotRegistered hand serv
            registerUser hand serv hname nick uname fname


extractLeft :: Either a b -> a
extractLeft (Left x) = x
extractLeft _ = error "Either is not left"

extractRight :: Either a b -> b
extractRight (Right x) = x
extractRight _ = error "Either is not right"

sendUnknownCmd :: Handle -> S.Server -> IO ()
sendUnknownCmd hand serv = do
  let pref = M.ServerName (S.hostname serv)
  let rep = R.Reply pref 421 ["*", "*"] "Unknown Command"
  R.sendReply hand rep

sendNoNickGiven :: Handle -> S.Server -> IO ()
sendNoNickGiven hand serv = do
  let pref = M.ServerName (S.hostname serv)
  let rep = R.Reply pref 431 ["*"] "No Nick Given"
  R.sendReply hand rep

sendBadNick :: Handle -> S.Server -> IO ()
sendBadNick hand serv = do
  let pref = M.ServerName (S.hostname serv)
  let rep = R.Reply pref 432 ["*"] "Erroneous nickname"
  R.sendReply hand rep

sendWelcome :: S.Server -> MVar U.User -> IO ()
sendWelcome serv useVar = do
  use <- takeMVar useVar
  let pref = M.ServerName (S.hostname serv)
  let rep = R.Reply pref 1 [U.nickname use] (U.genWelcomeMessage use)
  R.sendReply (U.handle use) rep
  putMVar useVar use

sendNeedMoreParams :: Handle -> S.Server -> String -> IO ()
sendNeedMoreParams hand serv cmd = do
  let pref = M.ServerName (S.hostname serv)
  let rep = R.Reply pref 461 ["*", cmd] "Not enough parameters"
  R.sendReply hand rep

sendNickInUse :: Handle -> S.Server -> String -> IO ()
sendNickInUse hand serv nick = do
  let pref = M.ServerName (S.hostname serv)
  let rep = R.Reply pref 433 ["*", nick] "Nickname is already in use"
  R.sendReply hand rep

sendNotRegistered :: Handle -> S.Server -> IO ()
sendNotRegistered hand serv = do
  let pref = M.ServerName (S.hostname serv)
  let rep = R.Reply pref 451 ["*"] "You have not registered"
  R.sendReply hand rep

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

registerUser :: Handle -> S.Server -> Maybe String -> Maybe String -> Maybe String -> IO (Maybe U.User)
registerUser hand serv nick uname fname = do
  isHandClosed <- hIsEOF hand
  if isHandClosed then return Nothing
  else do
    cmd <- hGetLine hand
    let msgEither = M.parseMessage cmd
    case msgEither of
      Left _ -> sendUnknownCmd hand >> registerUser hand serv nick uname fname
      Right msg -> do
        case map toUpper (M.command msg) of
          "NICK" -> do
            if M.numParams msg < 1 then sendNoNickGiven hand >> registerUser hand serv nick uname fname
            else do
              let chosenNick = (M.params msg) !! 0
              if length chosenNick > 9 then sendBadNick hand >> registerUser hand serv nick uname fname
              else do
                nickMap <- takeMVar (S.nicks serv)
                if HM.member chosenNick nickMap then do
                  sendNickInUse hand chosenNick
                  putMVar (S.nicks serv) nickMap
                  registerUser hand serv nick uname fname
                else do
                  let newNickMap = HM.insert chosenNick () nickMap
                  putMVar (S.nicks serv) newNickMap
                  if isJust uname then do
                    let newUser = U.User chosenNick (fromJust uname) (fromJust fname) "localhost"
                    oldUserMap <- takeMVar (S.users serv)
                    let newUserMap = HM.insert chosenNick newUser oldUserMap
                    putMVar (S.users serv) newUserMap
                    sendWelcome hand newUser
                    return $ Just newUser
                  else do
                    registerUser hand serv (Just chosenNick) uname fname
          "USER" -> do
            if M.numParams msg < 4 then do
              sendNeedMoreParams hand "USER"
              registerUser hand serv nick uname fname
            else do
              let newUsername = (M.params msg) !! 0
              let newFullName = (M.params msg) !! 3
              if isJust nick then do
                let newUser = U.User (fromJust nick) newUsername newFullName "localhost"
                oldUserMap <- takeMVar (S.users serv)
                let newUserMap = HM.insert (fromJust nick) newUser oldUserMap
                putMVar (S.users serv) newUserMap
                sendWelcome hand newUser
                return $ Just newUser
              else registerUser hand serv nick (Just newUsername) (Just newFullName)
          _ -> do
            R.sendNotRegistered hand
            registerUser hand serv nick uname fname


extractLeft :: Either a b -> a
extractLeft (Left x) = x
extractLeft _ = error "Either is not left"

extractRight :: Either a b -> b
extractRight (Right x) = x
extractRight _ = error "Either is not right"

sendUnknownCmd :: Handle -> IO ()
sendUnknownCmd hand = do
  let pref = M.ServerName "localhost"
  let rep = R.Reply pref 421 ["*", "*"] "Unknown Command"
  R.sendReply hand rep

sendNoNickGiven :: Handle -> IO ()
sendNoNickGiven hand = do
  let pref = M.ServerName "localhost"
  let rep = R.Reply pref 431 ["*"] "No Nick Given"
  R.sendReply hand rep

sendBadNick :: Handle -> IO ()
sendBadNick hand = do
  let pref = M.ServerName "localhost"
  let rep = R.Reply pref 432 ["*"] "Erroneous nickname"
  R.sendReply hand rep

sendWelcome :: Handle -> U.User -> IO ()
sendWelcome hand use = do
  let pref = M.ServerName "localhost"
  let rep = R.Reply pref 1 [U.nickname use] (U.genWelcomeMessage use)
  R.sendReply hand rep

sendNeedMoreParams :: Handle -> String -> IO ()
sendNeedMoreParams hand cmd = do
  let pref = M.ServerName "localhost"
  let rep = R.Reply pref 461 ["*", cmd] "Not enough parameters"
  R.sendReply hand rep

sendNickInUse :: Handle -> String -> IO ()
sendNickInUse hand nick = do
  let pref = M.ServerName "localhost"
  let rep = R.Reply pref 433 ["*", nick] "Nickname is already in use"
  R.sendReply hand rep

module Command.Nick where

import Data.Char
import qualified Message as M
import qualified Reply as R
import qualified Server as S
import qualified User as U
import Control.Concurrent.MVar
import Command.Helper
import Utility
import Data.HashMap.Lazy as HM
import System.IO

changeNick :: S.Server -> MVar U.User -> M.Message -> IO ()
changeNick serv useVar msg
  | M.numParams msg < 1 = sendNeedMoreParams useVar serv "NICK"
  | not (validateNick (head (M.params msg))) = sendBadNick useVar serv (head (M.params msg))
  | otherwise = do
      let chosenNick = head (M.params msg)
      oldUser <- takeMVar useVar
      putMVar useVar oldUser
      let oldNick = U.nickname oldUser
      nickMap <- takeMVar (S.nicks serv)
      if HM.member chosenNick nickMap then do
        putMVar (S.nicks serv) nickMap
        sendNickInUse useVar serv chosenNick
      else do
        let newNickMap = HM.delete oldNick nickMap
        putMVar (S.nicks serv) $ HM.insert chosenNick () newNickMap
        userMap <- takeMVar (S.users serv)
        let removedMap = HM.delete oldNick userMap
        let addedMap = HM.insert chosenNick useVar removedMap
        putMVar (S.users serv) addedMap
        let newUser = oldUser {U.nickname = chosenNick}
        _ <- takeMVar useVar
        putMVar useVar newUser
        let nickMsgPrefix = ':' : (oldNick ++ "!" ++ (U.username newUser) ++ "@" ++ (U.hostname newUser))
        let nickMsgCmd = " NICK "
        let nickMsgParam = ':' : chosenNick
        let nickmsg = nickMsgPrefix ++ nickMsgCmd ++ nickMsgParam
        writeUser <- takeMVar useVar
        sendSafe (U.handle writeUser) nickmsg
        putMVar useVar writeUser


isSpec :: Char -> Bool
isSpec c = (cVal >= 0x5B && cVal <= 0x60) || (cVal >= 0x7B && cVal <= 0x7D) where
  cVal = ord c

isValidMiddleChar :: Char -> Bool
isValidMiddleChar c = isLetter c || isDigit c || isSpec c || c == '-'

validateNick :: String -> Bool
validateNick [] = False
validateNick (x:xs)
  | isLetter x || isSpec x = validateNickHelper xs 8
  | otherwise = False where
    validateNickHelper _ 0 = False
    validateNickHelper [] _ = True
    validateNickHelper (c:cs) k
      | isValidMiddleChar c = validateNickHelper cs (k-1)
      | otherwise = False

sendBadNick :: MVar U.User -> S.Server -> String -> IO ()
sendBadNick useVar serv nick = do
  use <- takeMVar useVar
  let pref = M.ServerName (S.hostname serv)
  let rep = R.Reply pref 432 [U.nickname use, nick] "Erroneous nickname"
  R.sendReply (U.handle use) rep
  putMVar useVar use

sendNickInUse :: MVar U.User -> S.Server -> String -> IO ()
sendNickInUse useVar serv nick = do
  use <- takeMVar useVar
  let pref = M.ServerName (S.hostname serv)
  let rep = R.Reply pref 433 [U.nickname use, nick] "Nickname is already in use"
  R.sendReply (U.handle use) rep
  putMVar useVar use

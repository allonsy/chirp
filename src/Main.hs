module Main where

import Network
import System.IO
import GHC.IO.Handle
import Control.Concurrent
import Message
import Reply
import qualified Register as R
import Data.Maybe
import qualified Server as S
import qualified Data.HashMap.Lazy as HM
import Control.Concurrent.MVar
import Dispatch

portNumber = "6667"

main :: IO ()
main = withSocketsDo $ do
  nickMap <- newMVar HM.empty
  userMap <- newMVar HM.empty
  chanMap <- newMVar HM.empty
  let newServer = S.Server "localhost" userMap nickMap chanMap
  sock <- listenOn (Service portNumber)
  acceptConnections sock newServer


acceptConnections :: Socket -> S.Server -> IO ()
acceptConnections sock serv = do
  (hand, hname, _) <- accept sock
  hSetNewlineMode hand (NewlineMode CRLF CRLF)
  forkIO (handleClient hand serv hname)
  acceptConnections sock serv

handleClient :: Handle -> S.Server -> String -> IO ()
handleClient hand serv hname = do
  newUserMaybe <- R.registerUser hand serv hname Nothing Nothing Nothing
  if isJust newUserMaybe then do
    let newUser = fromJust newUserMaybe
    dispatch serv newUser hand
  else do
    putStrLn "client closed connection before completing registration"
    hClose hand

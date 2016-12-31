module Main where

import Network
import System.IO
import GHC.IO.Handle
import Control.Concurrent
import Message
import Reply
import Register as R
import Data.Maybe
import Server as S
import qualified Data.HashMap.Lazy as HM
import Control.Concurrent.MVar

portNumber = "6667"

main :: IO ()
main = withSocketsDo $ do
  nickMap <- newMVar HM.empty
  userMap <- newMVar HM.empty
  let newServer = S.Server "localhost" userMap nickMap
  sock <- listenOn (Service portNumber)
  acceptConnections sock newServer


acceptConnections :: Socket -> S.Server -> IO ()
acceptConnections sock serv = do
  (hand, _, _) <- accept sock
  hSetNewlineMode hand (NewlineMode CRLF CRLF)
  forkIO (handleClient hand serv)
  acceptConnections sock serv

handleClient :: Handle -> S.Server -> IO ()
handleClient hand serv = do
  newUserMaybe <- R.registerUser hand serv Nothing Nothing Nothing
  if isJust newUserMaybe then do
    let newUser = fromJust newUserMaybe
    putStrLn "registered User"
    _ <- hGetLine hand
    return ()
  else do
    putStrLn "client closed connection before completing registration"

module Main where

import Network
import System.IO
import GHC.IO.Handle
import Control.Concurrent
import Message
import Reply
import Register as R
import Data.Maybe

portNumber = "6667"

main :: IO ()
main = withSocketsDo $ do
  sock <- listenOn (Service portNumber)
  acceptConnections sock


acceptConnections :: Socket -> IO ()
acceptConnections sock = do
  (hand, _, _) <- accept sock
  hSetNewlineMode hand (NewlineMode CRLF CRLF)
  forkIO (handleClient hand)
  acceptConnections sock

handleClient :: Handle -> IO ()
handleClient hand = do
  newUserMaybe <- R.registerUser hand Nothing Nothing Nothing
  if isJust newUserMaybe then do
    let newUser = fromJust newUserMaybe
    putStrLn "registered User"
    hClose hand
  else do
    putStrLn "Client closed before registering"
    hClose hand

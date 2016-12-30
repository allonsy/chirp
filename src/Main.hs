module Main where

import Network
import System.IO
import GHC.IO.Handle
import Control.Concurrent
import Message
import Reply

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
  isHandClosed <- hIsEOF hand
  if isHandClosed then do
    putStrLn "Closing client"
    hClose hand
  else do
    cmd <- hGetLine hand
    putStrLn $ "received message: " ++ cmd
    handleClient hand

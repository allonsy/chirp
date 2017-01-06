module Channel where

import qualified Data.HashMap.Lazy as HM
import qualified User as U
import Control.Concurrent.MVar
import Control.Concurrent
import Utility

data Channel = Channel {
  topic :: MVar String,
  users :: MVar [MVar U.User]
}

-- sendToChannel takes the channel, a user to not send the message to, and the message itself and sends to all users
-- but the given one
sendToChannel :: Channel -> MVar U.User -> String -> IO ()
sendToChannel chan origin msg = do
  users <- readMVar (users chan)
  let dumpRes _ = return ()
  _ <- forkFinally (sendToChannelAsync users origin msg) dumpRes
  return ()


sendToChannelAsync :: [MVar U.User] -> MVar U.User -> String -> IO ()
sendToChannelAsync users origin msg = mapM_ sendToUser users
  where
    sendToUser use = if use == origin then return ()
      else do
        targetUser <- takeMVar use
        sendSafe (U.handle targetUser) msg

module Register where

import qualified User as U
import qualified Message as M
import qualified Reply as R
import System.IO
import Data.Char

registerUser :: Handle -> IO U.User
registerUser hand = do
  cmd <- hGetLine hand
  let msgEither = M.parseMessage cmd
  case msgEither of
    Left _ -> sendUnknownCmd hand >> registerUser hand
    Right msg -> do
      case map toUpper (M.command msg) of
        "NICK" -> 

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

module User where

import qualified Message as M
import System.IO
import Data.Int
import Data.HashMap.Lazy as HM

data UserChannel = UserChannel {
  channelName :: String
}

data User = User {
  nickname :: String,
  username :: String,
  fullname :: String,
  hostname :: String,
  channels :: HM.HashMap String UserChannel,
  handle :: Handle
}

genPrefix :: User -> M.Prefix
genPrefix (User n u _ h _ _) = M.UserName n (Just u) (Just h)

genWelcomeMessage :: User -> String
genWelcomeMessage user = "Welcome to the Internet Relay Network " ++ (M.prefixToString (genPrefix user))

module User where

import qualified Message as M
import System.IO

data User = User {
  nickname :: String,
  username :: String,
  fullname :: String,
  hostname :: String,
  handle :: Handle
}

genPrefix :: User -> M.Prefix
genPrefix (User n u f h _) = M.UserName n (Just u) (Just h)

genWelcomeMessage :: User -> String
genWelcomeMessage user = "Welcome to the Internet Relay Network " ++ (M.prefixToString (genPrefix user))

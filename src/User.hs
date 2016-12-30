module User where

import qualified Message as M

data User = User {
  nickname :: String,
  username :: String,
  fullname :: String,
  hostname :: String
}

genPrefix :: User -> M.Prefix
genPrefix (User n u f h) = M.UserName n (Just u) (Just h)

genWelcomeMessage :: User -> String
genWelcomeMessage user = "Welcome to the Internet Relay Network " ++ (M.prefixToString (genPrefix user))

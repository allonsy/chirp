module Server where

import Data.HashMap.Lazy as HM
import User as U
import Control.Concurrent.MVar

data Server = Server {
  hostname :: String,
  users :: MVar (HM.HashMap String (MVar U.User)),
  nicks :: MVar (HM.HashMap String ())
}

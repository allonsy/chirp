module Server where

import Data.HashMap.Lazy as HM
import User as U
import Channel as C
import Control.Concurrent.MVar
import Data.Int

data Server = Server {
  hostname :: String,
  users :: MVar (HM.HashMap String (MVar U.User)),
  nicks :: MVar (HM.HashMap String ()),
  channels :: MVar (HM.HashMap String C.Channel)
}

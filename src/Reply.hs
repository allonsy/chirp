module Reply where

import System.IO
import qualified Message as M

data Reply = Reply {
  prefix :: M.Prefix,
  number :: Int,
  params :: [String],
  message :: String
}

threeDigit :: Int -> String
threeDigit n
  | n <= 9 = "00" ++ (show n)
  | n <= 99 = "0" ++ (show n)
  | otherwise = show n

instance Show Reply where
  show (Reply pref n p m) = M.prefixToString pref ++ " " ++ (threeDigit n) ++ paramString ++ " :" ++ m
    where
    paramString = concat $ map (\s -> ' ' : s) p


sendReply :: Handle -> Reply -> IO ()
sendReply hand rep = hPutStrLn hand (show rep)

sendNotRegistered :: Handle -> IO ()
sendNotRegistered hand = do
  let pref = M.ServerName "localhost"
  let rep = Reply pref 451 ["*"] "You have not registered"
  sendReply hand rep

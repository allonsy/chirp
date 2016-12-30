module Message where

import Data.Char

data Prefix = ServerName String | UserName {
  nick :: String,
  username :: Maybe String,
  hostname :: Maybe String
} deriving (Show)

data CmdErr = EmptyMessage | MessageError String deriving (Show)

data Message = Message {
  prefix :: Maybe Prefix,
  command :: String,
  params :: [String]
} deriving (Show)

numParams :: Message -> Int
numParams msg = length (params msg)

prefixToString :: Prefix -> String
prefixToString (ServerName sname) = ':' : (sname)
prefixToString (UserName n u h) = ":" ++ n ++ userString ++ hostString
  where
  userString = case u of
    Nothing -> ""
    Just uname -> '!' : uname
  hostString = case h of
    Nothing -> ""
    Just hname -> '@' : hname


breakAt :: Char -> String -> (String, String)
breakAt _ [] = ([], [])
breakAt c (x:xs)
  | c == x = ([], xs)
  | otherwise = (x:first, second) where
      (first, second) = breakAt c xs

-- Parses prefix and returns the prefix and the rest of the string or returns an error if no parse can be made
parsePrefix :: String -> Either CmdErr (Prefix, String)
parsePrefix str = resPrefix >>= (\val -> Right (val, rest)) where
  first = (words str) !! 0
  rest = stripWhiteLead $ drop (length first) str
  (nickname, afterNick) = breakAt '!' first
  (username, hostname)
    | afterNick == "" = ("", snd (breakAt '@' nickname))
    | otherwise = breakAt '@' afterNick
  resPrefix = case [nickname, username, hostname] of
    ["", _, _] -> Left $ MessageError "Invalid Prefix"
    [nick, "", ""] -> Right $ UserName nick Nothing Nothing
    [nick, "", host@(c:cs)] -> Right $ UserName nick Nothing (Just host)
    [nick, user@(x:xs), host@(c:cs)] -> Right $ UserName nick (Just user) (Just host)
    _ -> Left $ MessageError "Invalid Prefix"

parseWord :: String -> (String, String)
parseWord "" = ("", "")
parseWord str = (firstWord, rest) where
  firstWord = head $ words str
  rest = stripWhiteLead $ drop (length firstWord) str

parseParams :: String -> [String]
parseParams "" = []
parseParams (':':rest) = [rest]
parseParams str = firstParam : restParams where
  (firstParam, restString) = parseWord str
  restParams = parseParams restString

stripWhiteLead :: String -> String
stripWhiteLead [] = []
stripWhiteLead (c:cs)
  | isSpace c = stripWhiteLead cs
  | otherwise = c:cs

parseMessage :: String -> Either CmdErr Message
parseMessage str = case stripWhiteLead str of
  [] -> Left EmptyMessage
  (':':rest) -> (parsePrefix rest) >>= (\(pref, afterPrefix) -> Right (createMessageWithPrefix (Just pref) afterPrefix))
  restString -> Right $ createMessageWithPrefix Nothing restString
  where
    parseCommandAndParams cmdString = (command, params) where
      (command, restString) = parseWord cmdString
      params = parseParams restString
    createMessageWithPrefix pref after = Message pref cmd par where
      (cmd, par) = parseCommandAndParams after

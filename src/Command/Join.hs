module Command.Join where

import qualified Channel as C
import qualified Server as S
import qualified User as U
import qualified Message as M
import Data.List
import Data.Char


isValidChanName :: String -> Bool
isValidChanName name
  | length name > 50 = False
  | length name < 2 = False
  | otherwise = hasValidFirst (head name) && hasValidRest (tail name)

hasValidFirst :: Char -> Bool
hasValidFirst c
  | c == '#' = True
  | c == '+' = True
  | c == '&' = True
  | otherwise = False

hasValidMiddle :: Int -> Bool
hasValidMiddle c
  | c >= 0x01 && c <= 0x06 = True
  | c >= 0x08 && c <= 0x09 = True
  | c >= 0x0B && c <= 0x0C = True
  | c >= 0x0E && c <= 0x1F = True
  | c >= 0x21 && c <= 0x2B = True
  | c >= 0x2D && c <= 0x39 = True
  | c >= 0x3B && c <= 0xFF = True
  | otherwise = False

hasValidRest :: String -> Bool
hasValidRest chanName = and (map (hasValidMiddle . ord) chanName)

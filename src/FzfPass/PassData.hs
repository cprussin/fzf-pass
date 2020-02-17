module FzfPass.PassData
  ( PassData(PassData)
  , name
  , password
  , hasOtp
  , fields
  , parsePassData
  ) where

import Data.List (isPrefixOf)
import Data.Map (Map, fromList)

data PassData = PassData
  { name :: String
  , password :: Maybe String
  , hasOtp :: Bool
  , fields :: Map String String
  } deriving (Show)

parsePassData :: String -> String -> PassData
parsePassData passwordName out =
  PassData
    { name = passwordName
    , password = if passLine == "" then Nothing else Just passLine
    , hasOtp = any isOtpAuthLine outLines
    , fields = fromList $ map fieldPair $ fieldLines
    }
  where
    outLines = lines out
    passLine = outLines !! 0
    fieldLines = filter (not . isOtpAuthLine) $ tail outLines
    isOtpAuthLine str = "otpauth://" `isPrefixOf` str
    fieldPair line =
      ( takeWhile (/= ':') line
      , dropWhile (== ' ') $ tail $ dropWhile (/= ':') line
      )

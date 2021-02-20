{-# LANGUAGE MultiWayIf, FlexibleContexts #-}

module FzfPass.Pass
  ( PassData(PassData)
  , name
  , password
  , hasOtp
  , fields
  , ls
  , otp
  , parse
  ) where

import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO)
import Data.List (isPrefixOf, isSuffixOf)
import Data.Map (Map, fromList)
import System.Directory (listDirectory)
import System.Environment (getEnv)

import FzfPass.Error (Error(ReadPasswordError, OTPError, ListPasswordsError))
import FzfPass.Utils (readProcess', safeLiftIO)

data PassData = PassData
  { name :: String
  , password :: Maybe String
  , hasOtp :: Bool
  , fields :: Map String String
  } deriving (Show)

ls :: (MonadIO m, MonadError Error m) => m [String]
ls = safeLiftIO ListPasswordsError go
  where
    go = do
      passwordRoot <- getEnv "PASSWORD_STORE_DIR"
      map (passwordName passwordRoot) <$> findPasswordFiles passwordRoot
    passwordName root file =
      drop (length root + 1) $ take (length file - length ".gpg") file
    findPasswordFiles path = do
      files <- listDirectory path
      fmap concat <$> flip traverse files $ \file ->
        if | "." `isPrefixOf` file    -> pure []
           | ".gpg" `isSuffixOf` file -> pure [path <> "/" <> file]
           | otherwise                -> findPasswordFiles (path <> "/" <> file)

otp :: (MonadIO m, MonadError Error m) => PassData -> m String
otp passData = init <$> readProcess' err "pass" ["otp", passName] ""
  where
    err = OTPError passName
    passName = name passData

parse :: (MonadIO m, MonadError Error m) => String -> m PassData
parse entry = parsePassData <$> readProcess' err "pass" args ""
  where
    err = ReadPasswordError entry
    args = ["show", entry]
    parsePassData out =
      PassData
        { name = entry
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

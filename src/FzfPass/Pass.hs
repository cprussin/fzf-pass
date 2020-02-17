{-# LANGUAGE MultiWayIf, FlexibleContexts #-}

module FzfPass.Pass
  ( ls
  , otp
  , parse
  ) where

import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO)
import Data.List (isPrefixOf, isSuffixOf)
import System.Directory (listDirectory)
import System.Environment (getEnv)

import FzfPass.Error (Error(ReadPasswordError, OTPError, ListPasswordsError))
import FzfPass.PassData (PassData, parsePassData, name)
import FzfPass.Utils (readProcess', safeLiftIO)

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
parse entry = parsePassData entry <$> readProcess' err "pass" args ""
  where
    err = ReadPasswordError entry
    args = ["show", entry]

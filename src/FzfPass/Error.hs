{-# LANGUAGE LambdaCase #-}

module FzfPass.Error
  ( Error(..)
  , exitCode
  ) where

import Control.Exception (Exception, IOException)
import System.Exit (ExitCode(ExitSuccess, ExitFailure))

data Error
  = UserCancelled
  | ReadPasswordError String IOException
  | OTPError String IOException
  | ListPasswordsError IOException
  | BrowserError IOException
  | ClipboardError IOException
  | ClipboardClearError IOException
  | NoPasswordError String
  | NoFieldError String String
  | ForkFailedError IOException

instance Show Error where
  show = \case
    UserCancelled           -> ""
    ListPasswordsError err  -> "Failed to list password entries: " <> show err
    BrowserError err        -> "Error opening browser: " <> show err
    ClipboardError err      -> "Error copying data to clipboard: " <> show err
    ClipboardClearError err -> "Error clearing clipboard: " <> show err
    ForkFailedError err     -> "Forking command failed: " <> show err

    ReadPasswordError entry err ->
      "Failed to read entry data for entry '" <> entry <> "': " <> show err

    OTPError entry err ->
      "Couldn't get OTP code for entry '" <> entry <> "': " <> show err

    NoPasswordError entry ->
      "The entry '" <> entry <> "' had no password data"

    NoFieldError entry field ->
      "The entry '" <> entry <> "' has no field named '" <> field <> "'"

instance Exception Error

exitCode :: Error -> ExitCode
exitCode = \case
  UserCancelled         -> ExitSuccess
  ReadPasswordError _ _ -> ExitFailure 3
  OTPError _ _          -> ExitFailure 4
  ListPasswordsError _  -> ExitFailure 5
  BrowserError _        -> ExitFailure 6
  ClipboardError _      -> ExitFailure 7
  ClipboardClearError _ -> ExitFailure 8
  NoPasswordError _     -> ExitFailure 9
  NoFieldError _ _      -> ExitFailure 10
  ForkFailedError _     -> ExitFailure 11

{-# LANGUAGE LambdaCase, FlexibleContexts #-}

module Main
  ( main
  ) where

import Prelude
import Control.Monad (unless)
import Control.Monad.Except (MonadError, throwError)
import Control.Monad.Except (runExceptT)
import Control.Monad.IO.Class (MonadIO)
import Data.List (sort)
import Data.Map ((!?), keys)
import Data.Maybe (isJust)
import System.Exit (exitWith)

import FzfPass.Browser (browse)
import FzfPass.Error (Error(NoPasswordError, NoFieldError, ForkFailedError), exitCode)
import FzfPass.Fzf (fzf)
import FzfPass.Pass (PassData, fields, name, hasOtp, password, parse, ls, otp)
import FzfPass.Utils (nohup, safeLiftIO)
import FzfPass.WlClip (wlCopy, wlCopyWithTimeout)

data Command = ClipOTP | ClipPassword | GoToSite | ClipField String

main :: IO ()
main =
  withErrorHandling $ do
    passData <- ls >>= fzf >>= parse
    cmd <- prompt passData
    safeLiftIO ForkFailedError $ nohup $ withErrorHandling $ run passData cmd
  where
    withErrorHandling exceptT = runExceptT exceptT >>= either handleError pure
    handleError err = do
      let errorMessage = show err
      unless (errorMessage == "") $ putStrLn errorMessage
      exitWith $ exitCode err

prompt :: (MonadIO m, MonadError Error m) => PassData -> m Command
prompt passData = parseCommand <$> fzf promptFields
  where
    promptFields = sort $
      (if isJust $ password passData then ["Password"] else []) ++
      (if hasOtp passData then ["OTP"] else []) ++
      (if "URL" `elem` fieldKeys then ["Go to site"] else []) ++
      fieldKeys
    fieldKeys = keys (fields passData)
    parseCommand = \case
      "Password" -> ClipPassword
      "OTP" -> ClipOTP
      "Go to site" -> GoToSite
      input -> ClipField input

run :: (MonadIO m, MonadError Error m) => PassData -> Command -> m ()
run passData =
  \case
    ClipOTP -> otp passData >>= wlCopy
    ClipPassword -> safely (NoPasswordError name') clipPassword
    GoToSite -> safely (NoFieldError name' "URL") $ browse <$> readField "URL"
    ClipField field -> safely (NoFieldError name' field) $ clipField field
  where
    clipPassword = wlCopyWithTimeout 30 <$> password passData
    clipField field = wlCopy <$> readField field
    safely err = maybe (throwError err) id
    readField field = fields passData !? field
    name' = name passData

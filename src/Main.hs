{-# LANGUAGE LambdaCase #-}

module Main
  ( main
  ) where

import Prelude
import Control.Monad (unless)
import Control.Monad.Except (runExceptT)
import System.Exit (exitWith)

import FzfPass.Command (prompt, run)
import FzfPass.Error (Error(ForkFailedError), exitCode)
import FzfPass.Fzf (fzf)
import FzfPass.Pass (parse, ls)
import FzfPass.Utils (nohup, safeLiftIO)

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

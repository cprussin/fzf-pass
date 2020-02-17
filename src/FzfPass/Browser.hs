{-# LANGUAGE FlexibleContexts #-}

module FzfPass.Browser
  ( browse
  ) where

import Control.Monad (void)
import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO)
import System.Environment (getEnv)
import System.Process (spawnProcess)

import FzfPass.Error (Error(BrowserError))
import FzfPass.Utils (safeLiftIO)

browse :: (MonadIO m, MonadError Error m) => String -> m ()
browse url = safeLiftIO BrowserError $ void $ do
  browser <- getEnv "BROWSER"
  spawnProcess browser [url]

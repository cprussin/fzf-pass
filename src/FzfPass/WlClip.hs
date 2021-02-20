{-# LANGUAGE FlexibleContexts #-}

module FzfPass.WlClip
  ( wlCopy
  , wlCopyWithTimeout
  ) where

import Control.Concurrent (threadDelay)
import Control.Monad (when, void)
import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO)
import System.Process (spawnProcess, readProcess)

import FzfPass.Error (Error(ClipboardError, ClipboardClearError))
import FzfPass.Utils (safeLiftIO)

wlCopy :: (MonadIO m, MonadError Error m) => String -> m ()
wlCopy value =
  safeLiftIO ClipboardError $ void $ spawnProcess "wl-copy" [value]

wlClear :: (MonadIO m, MonadError Error m) => Int -> String -> m ()
wlClear timeout value = safeLiftIO ClipboardClearError $ do
  threadDelay $ timeout * 1000000
  current <- init <$> readProcess "wl-paste" [] ""
  when (current == value) $ void $ spawnProcess "wl-copy" ["--clear"]

wlCopyWithTimeout :: (MonadIO m, MonadError Error m) => Int -> String -> m ()
wlCopyWithTimeout timeout value = void $ do
  wlCopy value
  wlClear timeout value

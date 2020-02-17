{-# LANGUAGE FlexibleContexts #-}

module FzfPass.Fzf
  ( fzf
  ) where

import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO)

import FzfPass.Error (Error(UserCancelled))
import FzfPass.Utils (readProcess')

fzf :: (MonadIO m, MonadError Error m) => [String] -> m String
fzf options = init <$> readProcess' (const UserCancelled) "fzf" args stdin
  where
    args = words "--layout reverse"
    stdin = unlines options

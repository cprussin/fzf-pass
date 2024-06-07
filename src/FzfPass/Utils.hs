{-# LANGUAGE ScopedTypeVariables #-}

module FzfPass.Utils
  ( readProcess'
  , safeLiftIO
  , nohup
  ) where

import Control.Exception (IOException, handle)
import Control.Monad (void)
import Control.Monad.Except (MonadError, liftEither)
import Control.Monad.IO.Class (MonadIO, liftIO)
import System.Exit (ExitCode(ExitSuccess))
import System.Posix (OpenMode(ReadWrite), openFd, closeFd, dupTo, defaultFileFlags, stdInput, stdOutput, stdError)
import System.Posix.Process (createSession, forkProcess, exitImmediately)
import System.Process (readProcess)

safeLiftIO :: (MonadIO m, MonadError e m) => (IOException -> e) -> IO a -> m a
safeLiftIO err io = go >>= liftEither
  where
    go = liftIO $ handle handleError $ Right <$> io
    handleError (e :: IOException) = pure $ Left $ err e

readProcess'
  :: (MonadIO m, MonadError e m)
  => (IOException -> e)
  -> FilePath
  -> [String]
  -> String
  -> m String
readProcess' err cmd args stdin = safeLiftIO err $ readProcess cmd args stdin

nohup :: IO () -> IO ()
nohup process =
  void $ forkProcess $ do
    _ <- createSession
    _ <- forkProcess $ do
      devnull <- openFd "/dev/null" ReadWrite defaultFileFlags
      mapM_ (\fd -> closeFd fd >> dupTo devnull fd) [stdInput, stdOutput, stdError]
      process
    exitImmediately ExitSuccess

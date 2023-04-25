-- |

module HydraPay.Utils where

import Data.Text (Text, pack)
import System.Exit
import System.Process
import Control.Monad.IO.Class
import Control.Concurrent.STM

tShow :: Show a => a -> Text
tShow = pack . show

eitherReadProcess :: MonadIO m => CreateProcess -> m (Either String String)
eitherReadProcess cp = do
  (code, out, err) <- liftIO $ readCreateProcessWithExitCode cp ""
  case code of
    ExitSuccess -> pure $ Right out
    ExitFailure _ -> pure $ Left err

withTMVar :: MonadIO m => TMVar a -> (a -> m (a, b)) -> m b
withTMVar var action = do
  val <- liftIO $ atomically $ takeTMVar var
  (a, b) <- action val
  liftIO $ atomically $ putTMVar var a
  pure b

headMaybe :: [a] -> Maybe a
headMaybe [] = Nothing
headMaybe (x:_) = Just x

maybeToEither :: a -> Maybe b -> Either a b
maybeToEither _ (Just b)  = Right b
maybeToEither a _ = Left a

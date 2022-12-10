-- |

module Common.Helpers where

import System.Exit (ExitCode(..))
import Data.Text (Text, pack)

tShow :: Show a => a -> Text
tShow = pack . show

headMay :: [a] -> Maybe a
headMay (a:_) = Just a
headMay _ = Nothing

seconds :: Int -> Int
seconds = (* 1000000)

-- | Turn a process call like 'readCreateProcessWithExitCode' or 'readProcessWithExitCode' into
-- an Either where success when the ExitCode is ExitSuccess
processAdapter :: Monad m => m (ExitCode, String, String) -> m (Either String String)
processAdapter action = do
  (exitCode, out, err) <- action
  pure $ case exitCode of
    ExitSuccess -> Right out
    _ -> Left err

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe = either (const Nothing) Just

mapBoth :: (a -> c) -> (b -> d) -> Either a b -> Either c d
mapBoth f _ (Left x)  = Left (f x)
mapBoth _ f (Right x) = Right (f x)

mapLeft :: (a -> c) -> Either a b -> Either c b
mapLeft f = mapBoth f id

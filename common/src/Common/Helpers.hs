-- |

module Common.Helpers where

import Data.Text (Text, pack)

tShow :: Show a => a -> Text
tShow = pack . show

headMay :: [a] -> Maybe a
headMay (a:_) = Just a
headMay _ = Nothing

seconds :: Int -> Int
seconds = (* 1000000)

-- |

module Common.Helpers where

headMay :: [a] -> Maybe a
headMay (a:_) = Just a
headMay _ = Nothing

seconds :: Int -> Int
seconds = (* 1000000)

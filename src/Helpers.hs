module Helpers (safeIndex, safeHead, makeMaybe) where

-- A safe version of (!!) that returns Nothing if the position doesn't exist
safeIndex :: Int -> [a] -> Maybe a
safeIndex _ []       = Nothing
safeIndex 0 (x : _)  = Just x
safeIndex i (_ : xs) = safeIndex (i - 1) xs

safeHead :: [a] -> Maybe a
safeHead = safeIndex 0

makeMaybe :: Bool -> a -> Maybe a
makeMaybe False _ = Nothing
makeMaybe True a = Just a


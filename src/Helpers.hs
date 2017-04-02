module Helpers (index) where

-- A safe version of (!!) that returns Nothing if the position doesn't exist
index :: Int -> [a] -> Maybe a
index _ []       = Nothing
index 0 (x : _)  = Just x
index i (_ : xs) = index (i - 1) xs


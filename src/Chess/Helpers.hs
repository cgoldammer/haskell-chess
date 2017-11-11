module Chess.Helpers (
    safeIndex
  , safeHead
  , makeMaybe
  , makeEither
  , maybeToEither) where

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

makeEither :: Bool -> a -> b -> Either a b
makeEither True a' _ = Left a'
makeEither False _ b' = Right b'

maybeToEither :: Maybe a -> b -> Either a b
maybeToEither Nothing b' = Right b'
maybeToEither (Just a') _ = Left a'


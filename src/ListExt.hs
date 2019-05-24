module ListExt (
  safeHead,
  safeLast
) where

-- TODO use lib
safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (a : _) = Just a

safeLast :: [a] -> Maybe a
safeLast [] = Nothing
safeLast [a] = Just a
safeLast (_ : as) = safeLast as
module HArch.HArchError where

data HArchError = FailedToLoadStore String deriving (Eq, Show)
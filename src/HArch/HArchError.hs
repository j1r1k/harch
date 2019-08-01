module HArch.HArchError where

-- TODO move ExceptT logic here

data HArchError = 
    FailedToLoadStore String 
  | FailedToLoadConfig String 
  | FailedToParseTemplate String
  | FailedToInterpolateConfig [String]
  | ArchiveNotFound String
  deriving (Eq, Show)

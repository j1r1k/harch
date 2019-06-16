module HArch.CmdArguments where

import Options.Applicative (Parser, (<|>))

import HArch.Configuration (CreateArchiveOptions, RestoreArchiveOptions)

data OperationMode = CreateArchiveMode CreateArchiveOptions
                   | RestoreArchiveMode RestoreArchiveOptions
                   deriving (Eq, Show)
            
parseCreateArchiveMode = undefined

parseRestoreArchiveMode = undefined

parseOperationMode :: Parser OperationMode
parseOperationMode = parseCreateArchiveMode <|> parseRestoreArchiveMode
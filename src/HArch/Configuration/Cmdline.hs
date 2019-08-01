module HArch.Configuration.Cmdline where

import Data.Text (Text)

import HArch.Metadata (ArchiveType(..))
import HArch.Path (Path(..))
import HArch.Configuration.Mode (HArchMode(..), CreateArchiveConfig(..), RestoreArchiveConfig(..))

import Options.Applicative (Parser, (<**>), argument, str, command, fullDesc, progDesc, header, execParser, subparser, help, info, short, long, metavar, optional, option, maybeReader, strOption, helper)

data HArchCmdlineConfiguration =
  HArchCmdlineConfiguration {
    config :: Maybe Path,
    mode :: HArchMode
  } deriving (Eq, Show)

-- TODO should be generic
readArchiveType :: String -> Maybe ArchiveType
readArchiveType "full" = pure Full
readArchiveType "incremental" = pure Incremental
readArchiveType _ = Nothing

parserName :: Parser Text
parserName = strOption
        ( short 'n'
        <> long "name"
        <> metavar "NAME"
        <> help "Archive name"
        )

parserCreateArchiveConfig :: Parser CreateArchiveConfig
parserCreateArchiveConfig = 
  CreateArchiveConfig
    <$> argument str 
        ( metavar "SOURCE"
        <> help "Source to be archived")
    <*> optional parserName
    <*> optional (option (maybeReader readArchiveType)
        ( short 't'
        <> long "type"
        <> metavar "ARCHIVE_TYPE"
        <> help "Archive type [full,incremental]" -- todo generic
        ))

parserRestoreArchiveConfig :: Parser RestoreArchiveConfig
parserRestoreArchiveConfig =
  RestoreArchiveConfig
    <$> parserName
    <*> argument str
        ( metavar "TARGET"
        <> help "Restore to target")

parserHArchMode :: Parser HArchMode
parserHArchMode = 
  subparser 
    ( command "create" (info (CreateArchiveMode <$> parserCreateArchiveConfig) (progDesc "Create archive"))
    <> command "restore" (info (RestoreArchiveMode <$> parserRestoreArchiveConfig) (progDesc "Restore archive"))
    )

parserHArchCmdlineConfiguration :: Parser HArchCmdlineConfiguration
parserHArchCmdlineConfiguration =
  HArchCmdlineConfiguration
    <$> optional (strOption
        ( short 'c'
        <> long "config"
        <> metavar "FILE"
        <> help "Path to configuration file"))
    <*> parserHArchMode

getCmdlineConfiguration :: IO HArchCmdlineConfiguration
getCmdlineConfiguration = execParser opts
  where opts = info (parserHArchCmdlineConfiguration <**> helper)
                    ( fullDesc 
                    <> progDesc "Create and restore archives of files" 
                    <> header "harch - archiving tool"
                    )

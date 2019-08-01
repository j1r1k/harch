{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
     where

import Control.Monad.Extra (forM)
import Control.Monad.Except (ExceptT(..), liftIO, runExceptT)
import Control.Error.Util (hoistEither)


import Data.Either.Extra (mapLeft, maybeToEither)
import qualified Data.Map.Strict as Map (lookup)
import Data.Maybe (fromMaybe)
import qualified Data.Text as Text (pack, unpack)
import Data.Time.LocalTime (LocalTime, utcToLocalTime, getCurrentTimeZone)
import Data.Time.Clock (getCurrentTime)
import qualified Data.UUID as UUID (toText)
import qualified Data.UUID.V4 as UUID (nextRandom)

import Turtle ((&))
import qualified Turtle as T (fromText)

import System.Environment (getEnv)
import System.Exit (ExitCode(..), exitFailure, exitWith)


import HArch.CombinedExitCode (CombinedExitCode(..))
import HArch.HArchError (HArchError(..))
import HArch.Path (Path(..), (</>), (<.>))
import HArch.Storage (SomeStorage, Storage(..), StorageConfig(..))
import HArch.ShellCommands (findFilesCmd, linesToBytes, tarCreateCmd, tarExtractCmd)
import HArch.Metadata (ArchiveMetadata(..), ArchiveType(..), archives)
import HArch.MetadataStore (ArchiveStore, addArchiveMetadata, lookupNewestArchiveMetadata, parseArchiveStore, selectArchivesToRestore, serializeArchiveStore)

import HArch.Configuration.General (HArchConfiguration(..), HArchGeneralConfig(..))
import HArch.Configuration.Storage (HArchStorageConfig(..))
import HArch.Configuration.Mode (CreateArchiveConfig(..), RestoreArchiveConfig(..), HArchMode(..))
import HArch.Configuration.Cmdline (HArchCmdlineConfiguration(..), getCmdlineConfiguration)
import HArch.Configuration.FileSource (loadHArchConfiguration)

-- TODO use UTC time
getCurrentLocalTime :: IO LocalTime
getCurrentLocalTime = utcToLocalTime <$> getCurrentTimeZone <*> getCurrentTime

data Storages = Storages {
    store :: SomeStorage,
    lists :: SomeStorage,
    files :: SomeStorage
}

makeStorages :: HArchStorageConfig -> Storages
makeStorages HArchStorageConfig { store, lists, files } = Storages {
    store = makeStorage store,
    lists = makeStorage lists,
    files = makeStorage files
}

type HarchOperation = HArchGeneralConfig -> Storages -> ArchiveStore -> ExceptT HArchError IO ExitCode

createOperation :: CreateArchiveConfig -> HarchOperation
createOperation createArchiveOptions generalConfiguration storages metadataStore = do
    currentLocalTime <- liftIO getCurrentLocalTime

    targetName <- case name (createArchiveOptions :: CreateArchiveConfig) of
        Nothing -> UUID.toText <$> liftIO UUID.nextRandom
        Just text -> pure text

    let targetPath = Path $ T.fromText targetName

    let currentPath = source createArchiveOptions

    let newestArchive = lookupNewestArchiveMetadata metadataStore currentPath
    let maybeNewest = case preferredArchiveType (generalConfiguration :: HArchGeneralConfig) 
                        of Full -> Nothing
                           Incremental -> time <$> newestArchive

    let actualArchiveType = fromMaybe Full $ const Incremental <$> maybeNewest

    liftIO $ print $ "Running " <> Text.pack (show actualArchiveType) <> " archive " <> targetName -- TODO logger

    let storeStorage = store (storages :: Storages)

    -- TODO replace with stream step after tarCreateCmd
    _ <- findFilesCmd currentPath maybeNewest 
        & linesToBytes
        & writeToFile storeStorage (targetPath <.> listExtension generalConfiguration)

    _ <- tarCreateCmd (tarOptions generalConfiguration) currentPath maybeNewest 
        & writeToFile (files (storages :: Storages)) (targetPath <.> archiveExtension generalConfiguration)

    let newBackupMetadata = ArchiveMetadata {
        archivePath = targetPath,
        archiveType = actualArchiveType,
        time = currentLocalTime
    }
    
    let newStore = addArchiveMetadata metadataStore currentPath newBackupMetadata

    pure newStore 
        & fmap serializeArchiveStore 
        & writeToFile storeStorage (storePath generalConfiguration)



restoreOperation :: RestoreArchiveConfig -> HarchOperation
restoreOperation restoreArchiveOptions generalConfiguration storages metadataStore = do
    let archiveName = name (restoreArchiveOptions :: RestoreArchiveConfig)
    let currentPath = Path $ T.fromText archiveName

    lookupResult <- hoistEither $ maybeToEither (ArchiveNotFound $ Text.unpack archiveName) $ Map.lookup currentPath metadataStore >>= (selectArchivesToRestore . archives) 

    liftIO $ print lookupResult

    codes <- liftIO $ forM lookupResult (\archive -> do
        print archive -- TODO
        exitCode <- readFromFile (files (storages :: Storages)) (archivePath archive <.> archiveExtension generalConfiguration)
            & tarExtractCmd (tarOptions generalConfiguration) (target (restoreArchiveOptions :: RestoreArchiveConfig))
        return $ CombinedExitCode exitCode
        )

    return $ unwrapExitCode $ mconcat codes

runOperation :: HArchConfiguration -> HArchMode -> ExceptT HArchError IO ExitCode
runOperation configuration mode' = do
    let storages = makeStorages $ storage configuration
    backupStore <- liftIO $ parseArchiveStore $ readFromFile (store (storages :: Storages)) (storePath $ general configuration)
    backupStore' <- hoistEither $ mapLeft (FailedToLoadStore . show) backupStore

    let operation = case mode' of CreateArchiveMode config' -> createOperation config'
                                  RestoreArchiveMode config' -> restoreOperation config'   

    operation (general configuration) storages backupStore'

runHarch :: IO (Either HArchError ExitCode)
runHarch = runExceptT $ do
    cmdlineConfiguration <- liftIO getCmdlineConfiguration
    home <- liftIO $ getEnv "HOME" -- TODO handle empty var

    let defaultConfigFile = Path (T.fromText $ Text.pack home) </> ".config" </> "harch" <.> "yaml" -- TODO put outside, add systemwide config
    let selectedConfigFile = fromMaybe defaultConfigFile $ config cmdlineConfiguration

    harchConfiguration <- loadHArchConfiguration selectedConfigFile
    runOperation harchConfiguration (mode cmdlineConfiguration)

someFunc :: IO ()
someFunc = do
    result <- runHarch
    case result of
        Right exitCode -> exitWith exitCode
        Left err -> do
            print err -- TODO
            exitFailure

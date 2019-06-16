{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE OverloadedStrings #-}
module Lib
     where

import Control.Monad.Extra (forM)

import qualified Data.Map.Strict as Map (lookup)
import Data.Maybe (fromMaybe)
import qualified Data.Text as Text (pack)
import Data.Time.LocalTime (LocalTime, utcToLocalTime, getCurrentTimeZone)
import Data.Time.Clock (getCurrentTime)
import qualified Data.UUID as UUID (toText)
import qualified Data.UUID.V4 as UUID (nextRandom)

import Turtle ((&))
import qualified Turtle as T (fromText)

import HArch.Path
import HArch.Storage
import HArch.ShellCommands
import HArch.Metadata
import HArch.MetadataStore

import HArch.Configuration.General
import HArch.Configuration.Storage
import HArch.Configuration.Modes

getCurrentLocalTime :: IO LocalTime
getCurrentLocalTime = utcToLocalTime <$> getCurrentTimeZone <*> getCurrentTime

data Storages = Storages {
    store :: SomeStorage,
    lists :: SomeStorage,
    files :: SomeStorage
}

makeStorages :: HArchStorageConfig -> Storages
makeStorages = undefined

type HarchOperation = HArchGeneralConfig -> Storages -> ArchiveStore -> IO ()

createOperation :: CreateArchiveConfig -> HarchOperation
createOperation createArchiveOptions generalConfiguration storages metadataStore = do
    currentLocalTime <- getCurrentLocalTime

    targetName <- case name (createArchiveOptions :: CreateArchiveConfig) of
        Nothing -> UUID.toText <$> UUID.nextRandom
        Just text -> pure text

    let targetPath = Path $ T.fromText targetName

    let currentPath = source createArchiveOptions

    let newestArchive = lookupNewestArchiveMetadata metadataStore currentPath
    let maybeNewest = case preferredArchiveType (generalConfiguration :: HArchGeneralConfig) 
                        of Full -> Nothing
                           Incremental -> time <$> newestArchive

    let actualArchiveType = fromMaybe Full $ const Incremental <$> maybeNewest

    print $ "Running " <> Text.pack (show actualArchiveType) <> " archive " <> targetName

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

    _ <- pure newStore 
        & fmap serializeArchiveStore 
        & writeToFile storeStorage (storePath generalConfiguration)

    print ("Done" :: String)


restoreOperation :: RestoreArchiveConfig -> HarchOperation
restoreOperation restoreArchiveOptions generalConfiguration storages metadataStore = do
    let currentPath = name (restoreArchiveOptions :: RestoreArchiveConfig)

    case Map.lookup currentPath metadataStore >>= (selectArchivesToRestore . archives) of
        Just archives' -> do
            print archives'

            _ <- forM archives' (\archive -> do
                print archive
                readFromFile (files (storages :: Storages)) (archivePath archive <.> archiveExtension generalConfiguration)
                    & tarExtractCmd (tarOptions generalConfiguration) (target (restoreArchiveOptions :: RestoreArchiveConfig)))

            return ()
        Nothing ->
            print ("No backup found" :: String)

runOperation :: HArchConfiguration -> HArchMode -> IO ()
runOperation configuration mode = do
    let storages = makeStorages $ storage configuration
    backupStore <- parseArchiveStore $ readFromFile (store (storages :: Storages)) (storePath $ general configuration)

    case backupStore of
        Right metadataStore -> 
            let operation = case mode of CreateArchiveMode config -> createOperation config
                                         RestoreArchiveMode config -> restoreOperation config   
             in operation (general configuration) storages metadataStore
        Left msg -> error $ "Cannot load backupStore " <> msg


sampleHarchConfiguration :: HArchConfiguration
sampleHarchConfiguration =
    HArchConfiguration {
        general = HArchGeneralConfig {
            storePath = "collection.json",
            listExtension = "list",
            archiveExtension = "bak",
            tarOptions = TarOptions { tarArgs = ["--acls", "--selinux", "--xattrs"] },
            preferredArchiveType = Incremental
        },
        storage = undefined 
    }

sampleCreateArchiveOptions :: CreateArchiveConfig
sampleCreateArchiveOptions = CreateArchiveConfig {
    name = Nothing,
    source = "/home/jirik/Desktop",
    preferredArchiveType = Nothing
}

sampleRestoreArchiveOptions :: RestoreArchiveConfig
sampleRestoreArchiveOptions = RestoreArchiveConfig {
    name = "src",
    target = "test/restore"
}

someFunc :: IO ()
{- 
    TODO

    parseCmdline arguments

    load config file (provided or default location(s))

    interpolate env variables in config file

    parse config file

    profit
-}
someFunc = runOperation sampleHarchConfiguration (RestoreArchiveMode sampleRestoreArchiveOptions)

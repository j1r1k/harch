{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
module Lib
     where

import Prelude hiding (readFile, writeFile)

import Control.Monad.Extra (forM)

import qualified Data.Map.Strict as Map (lookup)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text (pack)
import Data.Time.LocalTime (LocalTime, utcToLocalTime, getCurrentTimeZone)
import Data.Time.Clock (getCurrentTime)

import Turtle ((&))

import qualified Data.UUID as UUID (toText)
import qualified Data.UUID.V4 as UUID (nextRandom)

import Storage
import Storage.Local
import Storage.S3Cli
import Storage.Gpg
import ShellCommands
import Metadata
import MetadataStore
import ShellJSON

data StorageConfiguration s1 s2 s3 = StorageConfiguration {
    storeStorage :: Storage s1 => s1,
    filesStorage :: Storage s2 => s2,
    dataStorage :: Storage s3 => s3
}

sampleStorageConfiguration = StorageConfiguration {
    storeStorage = LocalStorage "test/meta",
    filesStorage = LocalStorage "test/meta/lists",
    dataStorage = GpgStorage { gpgOptions = gpgOptions, underlyingStorage = LocalStorage "test/backup" }
}
    where gpgOptions = GpgOptions { 
        gpgSign = True,
        gpgCipher = "AES256",
        gpgCompress = "BZIP2",
        gpgRecipient = "DF995C5E"
    }


data Configuration = Configuration {
    storeName :: Text,
    listExtension :: Text,
    archiveExtension :: Text,
    tarOptions :: TarOptions
} deriving (Eq, Show)

sampleConfiguration = Configuration {
    storeName = "collection.json",
    listExtension = ".list",
    archiveExtension = ".bak",
    tarOptions = TarOptions { tarArgs = ["--acls", "--selinux", "--xattrs"]}
}

data CreateArchiveOptions = CreateArchiveOptions {
    archiveName :: Text,
    archivePath :: Text,
    preferredArchiveType :: ArchiveType } deriving (Eq, Show)

sampleCreateArchiveOptions = do
    uuid <- UUID.toText <$> UUID.nextRandom

    return CreateArchiveOptions {
        archiveName = uuid,
        archivePath = "/home/jirik/Desktop",
        preferredArchiveType = Full
    }


getCurrentLocalTime :: IO LocalTime
getCurrentLocalTime = utcToLocalTime <$> getCurrentTimeZone <*> getCurrentTime

createFlow :: IO ()
createFlow = do
    let configuration = sampleConfiguration
    let storageConfiguration = sampleStorageConfiguration
    createArchiveOptions <- sampleCreateArchiveOptions

    backupStore <- parseArchiveStore $ readFile (storeStorage storageConfiguration) (storeName configuration)

    case backupStore of
        Right bs -> do
            currentLocalTime <- getCurrentLocalTime

            let currentName = archiveName createArchiveOptions
            let currentPath = archivePath createArchiveOptions

            let newestArchive = lookupNewestArchiveMetadata bs currentPath
            let maybeNewest = case preferredArchiveType createArchiveOptions 
                                of Full -> Nothing
                                   Incremental -> time <$> newestArchive

            let actualArchiveType = fromMaybe Full $ const Incremental <$> maybeNewest

            print $ "Running " <> Text.pack (show actualArchiveType) <> " archive " <> currentName

            findFilesCmd currentPath maybeNewest 
                & linesToBytes
                & writeFile (filesStorage storageConfiguration) (currentName <> listExtension configuration)

            tarCreateCmd (tarOptions configuration) currentPath maybeNewest 
                & writeFile (dataStorage storageConfiguration) (currentName <> archiveExtension configuration)

            let newBackupMetadata = ArchiveMetadata {
                archiveId = currentName,
                archiveType = actualArchiveType,
                time = currentLocalTime
            }
            
            let newStore = addArchiveMetadata bs currentPath newBackupMetadata

            _ <- pure newStore & fmap serializeArchiveStore & writeFile (storeStorage storageConfiguration) (storeName configuration)

            print ("Done" :: String)
        Left msg ->
            print $ "Wrong shape " <> msg

data RestoreArchiveOptions = RestoreArchiveOptions {
    restorePath :: Text,
    restoreTo :: Text
} deriving (Eq, Show)

sampleRestoreArchiveOptions = RestoreArchiveOptions {
    restorePath = "src",
    restoreTo = "test/restore"
}

restoreFlow :: IO ()
restoreFlow = do
    let configuration = sampleConfiguration
    let storageConfiguration = sampleStorageConfiguration
    let restoreArchiveOptions = sampleRestoreArchiveOptions

    backupStore <- parseArchiveStore $ readFile (storeStorage storageConfiguration) (storeName configuration)


    case backupStore of
        Right bs -> do
            let currentPath = restorePath restoreArchiveOptions

            case Map.lookup currentPath bs >>= (selectArchivesToRestore . archives) of
                Just archives -> do
                    print archives

                    _ <- forM archives (\archive -> do
                        print archive
                        readFile (dataStorage storageConfiguration) (archiveId archive <> archiveExtension configuration)
                            & tarExtractCmd (tarOptions configuration) (restoreTo restoreArchiveOptions)
                            )

                    return ()
                Nothing ->
                    print ("No backup found" :: String)
        Left msg ->
            print $ "Wrong shape " <> msg



someFunc :: IO ()
someFunc = restoreFlow

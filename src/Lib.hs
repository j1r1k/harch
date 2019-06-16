{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
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
import HArch.Configuration

getCurrentLocalTime :: IO LocalTime
getCurrentLocalTime = utcToLocalTime <$> getCurrentTimeZone <*> getCurrentTime

createFlow :: IO ()
createFlow = do
    let configuration = sampleConfiguration
    let storageConfiguration = sampleStorageConfiguration
    let createArchiveOptions = sampleCreateArchiveOptions

    backupStore <- parseArchiveStore $ readFromFile (storeStorage storageConfiguration) (storePath configuration)

    case backupStore of
        Right bs -> do
            currentLocalTime <- getCurrentLocalTime

            targetName <- case archiveIdentifier createArchiveOptions of
                UUIDArchiveIdentifier -> UUID.toText <$> UUID.nextRandom
                TextArchiveIdentifier text -> pure text

            let targetPath = Path $ T.fromText targetName

            let currentPath = createSource createArchiveOptions

            let newestArchive = lookupNewestArchiveMetadata bs currentPath
            let maybeNewest = case preferredArchiveType createArchiveOptions 
                                of Full -> Nothing
                                   Incremental -> time <$> newestArchive

            let actualArchiveType = fromMaybe Full $ const Incremental <$> maybeNewest

            print $ "Running " <> Text.pack (show actualArchiveType) <> " archive " <> targetName

            -- TODO replace with stream step after tarCreateCmd
            _ <- findFilesCmd currentPath maybeNewest 
                & linesToBytes
                & writeToFile (filesStorage storageConfiguration) (targetPath <.> listExtension configuration)

            _ <- tarCreateCmd (tarOptions configuration) currentPath maybeNewest 
                & writeToFile (dataStorage storageConfiguration) (targetPath <.> archiveExtension configuration)

            let newBackupMetadata = ArchiveMetadata {
                archiveId = targetPath,
                archiveType = actualArchiveType,
                time = currentLocalTime
            }
            
            let newStore = addArchiveMetadata bs currentPath newBackupMetadata

            _ <- pure newStore & fmap serializeArchiveStore & writeToFile (storeStorage storageConfiguration) (storeName configuration)

            print ("Done" :: String)
        Left msg ->
            print $ "Wrong shape " <> msg

restoreFlow :: IO ()
restoreFlow = do
    let configuration = sampleConfiguration
    let storageConfiguration = sampleStorageConfiguration
    let restoreArchiveOptions = sampleRestoreArchiveOptions

    backupStore <- parseArchiveStore $ readFromFile (storeStorage storageConfiguration) (storePath configuration)


    case backupStore of
        Right bs -> do
            let currentPath = toRestore restoreArchiveOptions

            case Map.lookup currentPath bs >>= (selectArchivesToRestore . archives) of
                Just archives -> do
                    print archives

                    _ <- forM archives (\archive -> do
                        print archive
                        readFromFile (dataStorage storageConfiguration) (archiveId archive <.> archiveExtension configuration)
                            & tarExtractCmd (tarOptions configuration) (restoreTo restoreArchiveOptions)
                            )

                    return ()
                Nothing ->
                    print ("No backup found" :: String)
        Left msg ->
            print $ "Wrong shape " <> msg



someFunc :: IO ()
someFunc = restoreFlow

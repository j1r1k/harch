{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}
module Lib
     where


import Control.Monad.Extra (forM)

import qualified Data.Map.Strict as Map (lookup)

import Data.Text (Text)
import Data.Time.LocalTime (LocalTime, utcToLocalTime, getCurrentTimeZone)
import Data.Time.Clock (getCurrentTime)

import Turtle ((&), (<.>), (</>))

import qualified Turtle as T (FilePath, fromText, output)

import qualified Data.UUID as UUID (toText)
import qualified Data.UUID.V4 as UUID (nextRandom)

import Storage
import Storage.Local
import Storage.S3Cli
import ShellCommands
import Metadata
import MetadataStore
import ShellJSON


import qualified Turtle.Bytes as TB (output, strict, input)

getCurrentLocalTime :: IO LocalTime
getCurrentLocalTime = utcToLocalTime <$> getCurrentTimeZone <*> getCurrentTime

data BackupArgs = BackupArgs {
    id :: Text,
    path :: FilePath,
    newer :: Maybe LocalTime } deriving (Eq, Show)

encryptFlow = do

    let backupOptions = TarOptions { tarArgs = ["--acls", "--selinux", "--xattrs"]}
    let encryptOptions = EncryptOptions { 
        gpgSign = True,
        gpgCipher = "AES256",
        gpgCompress = "BZIP2",
        gpgRecipient = "DF995C5E"
    }
    let backupMetaTarget :: T.FilePath = "/home/jirik/Downloads/bak/meta"

    let backupCollection = backupMetaTarget </> "collection.json"

    backupId <- UUID.toText <$> UUID.nextRandom
    let backupSource = "/home/jirik/Desktop"
    let backupTarget = "/home/jirik/Downloads/bak"
    let backupType = Incremental

    backupStore <- parseArchiveStore $ TB.input backupCollection

    case backupStore of
        Right bs -> do
            currentLocalTime <- getCurrentLocalTime

            let newestBackup = lookupNewestArchiveMetadata bs backupSource

            let maybeNewest = case backupType of Full -> Nothing
                                                 Incremental -> time <$> newestBackup

            print $ "Running backup " <> backupId
            print maybeNewest

            findFilesCmd backupSource maybeNewest & T.output (backupMetaTarget </> (T.fromText backupId) <.> "list")

            tarCreateCmd backupOptions backupSource maybeNewest 
                & gpgEncryptCmd encryptOptions
                & TB.output (backupTarget </> (T.fromText backupId) <.> "bak")

            let newBackupMetadata = ArchiveMetadata {
                archiveId = backupId,
                archiveType = backupType,
                time = currentLocalTime
            }

            (return $ serializeArchiveStore (addArchiveMetadata bs backupSource newBackupMetadata)) & TB.output backupCollection
        Left _ ->
            print ("Wrong shape" :: Text)


    --tarCmd backupOptions "/home/jirik/Desktop" Nothing 
    --    & gpgEncryptCmd encryptOptions
    --    & TB.output "/home/jirik/backup.tar"
    -- let lt = parseTimeM False defaultTimeLocale "%FT%T" "2019-05-21T12:00:00"
    -- empty & findCmd "/home/jirik/Desktop" lt & fmap lineToFileMetadata



decryptFlow = do
    let backupMetaTarget = "/home/jirik/Downloads/bak/meta"

    let backupCollection = backupMetaTarget </> "collection.json"

    let backupTarget = "/home/jirik/Downloads/bak"
    let restoreTarget = "/home/jirik/Downloads/bak.restore"
    let backupPath = "/home/jirik/Desktop"

    backupStore <- parseArchiveStore $ TB.input backupCollection

    let tarOptions = TarOptions { tarArgs = []}

    case backupStore of
        Right bs -> do
            case Map.lookup backupPath bs >>= (selectArchivesToRestore . archives) of
                Just backups -> do
                    print backups

                    _ <- forM backups (\backup@ArchiveMetadata {archiveId} -> do
                        print backup
                        TB.input (backupTarget </> (T.fromText archiveId) <.> "bak") & gpgDecryptCmd & tarExtractCmd tarOptions restoreTarget & TB.strict)

                    return ()

                Nothing ->
                    print ("No backup found" :: Text)
        Left _ ->
            print ("Wrong shape" :: Text)



someFunc :: IO ()
someFunc = decryptFlow

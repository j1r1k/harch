{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module HArch.Configuration where

import Data.Text (Text)

import HArch.Metadata
import HArch.Path (Path)
import HArch.Storage
import HArch.Storage.Gpg
import HArch.Storage.Local
import HArch.ShellCommands

data StorageConfiguration s1 s2 s3 = StorageConfiguration {
    storeStorage :: Storage s1 => s1,
    filesStorage :: Storage s2 => s2,
    dataStorage :: Storage s3 => s3
}

data Configuration = 
  Configuration {
    storePath :: Path,
    listExtension :: Text,
    archiveExtension :: Text,
    tarOptions :: TarOptions
  } deriving (Eq, Show)

data ArchiveIdentifier = 
    TextArchiveIdentifier Text 
  | UUIDArchiveIdentifier 
  deriving (Eq, Show)

data CreateArchiveOptions = CreateArchiveOptions {
    archiveIdentifier :: ArchiveIdentifier,
    createSource :: Path,
    preferredArchiveType :: ArchiveType } deriving (Eq, Show)

data RestoreArchiveOptions = RestoreArchiveOptions {
    toRestore :: Path,
    restoreTarget :: Path
} deriving (Eq, Show)

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

sampleConfiguration = Configuration {
    storePath = "collection.json",
    listExtension = "list",
    archiveExtension = "bak",
    tarOptions = TarOptions { tarArgs = ["--acls", "--selinux", "--xattrs"]}
}

sampleCreateArchiveOptions = CreateArchiveOptions {
    archiveIdentifier = UUIDArchiveIdentifier,
    createSource = "/home/jirik/Desktop",
    preferredArchiveType = Full
}

sampleRestoreArchiveOptions = RestoreArchiveOptions {
    toRestore = "src",
    restoreTarget = "test/restore"
}
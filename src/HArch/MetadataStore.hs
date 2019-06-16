module HArch.MetadataStore where

import qualified Data.Aeson.Encode.Pretty as A (encodePretty)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BS (toStrict)
import qualified Data.List as List (sort, sortBy)
import Data.Map (Map)
import qualified Data.Map as Map (fromList, insert, lookup, toAscList)

import Turtle (MonadIO, Shell)
import Safe (headMay, lastMay)

import HArch.Metadata (ArchiveCollection(..), ArchiveMetadata(..), ArchiveType(..))
import HArch.Path (Path)
import HArch.ShellJSON (fromJSON)

type ArchiveStore = Map Path ArchiveCollection

getNewestArchive :: ArchiveCollection -> Maybe ArchiveMetadata
getNewestArchive = lastMay . List.sort . archives

lookupNewestArchiveMetadata :: ArchiveStore -> Path -> Maybe ArchiveMetadata
lookupNewestArchiveMetadata store archiveRoot = Map.lookup archiveRoot store >>= getNewestArchive

-- TODO use lens
addArchiveMetadata :: ArchiveStore -> Path -> ArchiveMetadata -> ArchiveStore
addArchiveMetadata store archiveRoot newRecord = Map.insert archiveRoot newCollection store
    where newCollection = maybe ArchiveCollection { root = archiveRoot, archives = [newRecord] } 
                                (\collection -> collection { archives = newRecord : archives collection}) 
                                (Map.lookup archiveRoot store)

selectArchivesToRestore :: [ArchiveMetadata] -> Maybe [ArchiveMetadata]
selectArchivesToRestore backups = let (increments, rest) = span (\b -> Incremental == archiveType b) $ List.sortBy (flip compare) backups in (\full -> full : reverse increments) <$> headMay rest

-- TODO improve
parseArchiveStore :: MonadIO io => Shell ByteString -> io (Either String ArchiveStore)
parseArchiveStore = fmap (fmap (Map.fromList . fmap (\collection -> (root collection, collection)))) . fromJSON

serializeArchiveStore :: ArchiveStore -> ByteString
serializeArchiveStore = BS.toStrict . A.encodePretty . fmap snd . Map.toAscList
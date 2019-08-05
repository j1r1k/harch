# harch

Command line tool for archiving files and directories.

This is a work in progress effort.

## What it is?

It is essentially a script build in a haskell utilizing [turtle](http://hackage.haskell.org/package/turtle) library for shell programming in haskell.

It depends on standard unix tools `find`, `gpg` and `tar`.

It depends on [aws-cli](https://aws.amazon.com/cli/) when relying on AWS S3 for storage.

## Supported storages

- local
- AWS S3

## What it does?

It creates a manifest file listing a files that are stored. Then it creates a `tar` archive for the path, while optionally encrypting it via `gpg`. Archive is piped through to a storage.

It creates a `tar` archive while optionally encrypting it.

## Missing bits

- restore flow
  - allow restore of archive
  - search through manifests for a file/pattern to be restored and offer candidates for restore

### TODOs

- improve dependencies (e.g. containers x unordered-containers, mtl x transformers)
- improve logging / user information
- docs
- example configs
- tests
- auto format

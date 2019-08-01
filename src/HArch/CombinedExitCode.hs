module HArch.CombinedExitCode where

import System.Exit (ExitCode(..))

newtype CombinedExitCode = CombinedExitCode { unwrapExitCode :: ExitCode }

instance Semigroup CombinedExitCode where
  CombinedExitCode ExitSuccess <> CombinedExitCode ExitSuccess = CombinedExitCode ExitSuccess
  CombinedExitCode ExitSuccess <> right = right
  left <> CombinedExitCode ExitSuccess = left
  _ <> right = right

instance Monoid CombinedExitCode where
  mempty = CombinedExitCode ExitSuccess
module Euicc.Dir
    ( DirEntry
    , listDir
    ) where

-- \|
-- Module      : Euicc.Dir
-- Description : List a directory for the QR image picker
-- Copyright   : (c) Paolo Veronelli, 2026
-- License     : Apache-2.0
--
-- One small read-only step of the guided install: what files live in
-- a directory, so the operator can pick the purchase QR with the
-- cursor instead of typing a path. Directories are marked, dotfiles
-- are hidden, and nothing here touches the card.

import Control.Exception (IOException, try)
import Data.List (sortOn)
import Data.Text (Text)
import Data.Text qualified as T
import Euicc.Lpac.Output (LpacFailure (..))
import System.Directory
    ( doesDirectoryExist
    , listDirectory
    )
import System.FilePath ((</>))

-- | One entry of a listing: is it a directory, and its name.
type DirEntry = (Bool, Text)

-- | The entries of a directory, sorted by name, without dotfiles.
-- A missing or unreadable directory is a failure with the reason.
listDir :: FilePath -> IO (Either LpacFailure (FilePath, [DirEntry]))
listDir path = do
    r <- try $ listDirectory path
    case r of
        Left (e :: IOException) ->
            pure
                $ Left
                $ DirFailure
                $ T.pack
                $ "cannot list " <> path <> ": " <> show e
        Right names -> do
            entries <- mapM classify names
            pure
                $ Right
                    ( path
                    , sortOn snd $ filter (not . isDot . snd) entries
                    )
  where
    classify name =
        (, T.pack name) <$> doesDirectoryExist (path </> name)
    isDot name = T.isPrefixOf "." name

module Euicc.Dir
    ( DirEntry
    , listDir
    , isImage
    ) where

-- \|
-- Module      : Euicc.Dir
-- Description : List a directory for the QR image picker
-- Copyright   : (c) Paolo Veronelli, 2026
-- License     : Apache-2.0
--
-- One small read-only step of the guided install: what files live in
-- a directory, so the operator can pick the purchase QR with the
-- cursor instead of typing a path. Only directories and images are
-- listed, the most recently modified first, so a QR just saved is
-- under the cursor; dotfiles are hidden, and nothing here touches the
-- card.

import Control.Exception (IOException, try)
import Data.Char (toLower)
import Data.List (sortOn)
import Data.Ord (Down (..))
import Data.Text (Text)
import Data.Text qualified as T
import Euicc.Lpac.Output (LpacFailure (..))
import System.Directory
    ( doesDirectoryExist
    , getModificationTime
    , listDirectory
    , makeAbsolute
    )
import System.FilePath (takeExtension, (</>))

-- | One entry of a listing: is it a directory, and its name.
type DirEntry = (Bool, Text)

{- | The directories and images in a directory, the most recently
modified first (by name among equals), without dotfiles, with the directory's
absolute path, so that its parent is always reachable. A missing or
unreadable directory is a failure with the reason.
-}
listDir :: FilePath -> IO (Either LpacFailure (FilePath, [DirEntry]))
listDir relative = do
    path <- makeAbsolute relative
    r <- try $ listDirectory path
    case r of
        Left (e :: IOException) ->
            pure
                $ Left
                $ DirFailure
                $ T.pack
                $ "cannot list " <> path <> ": " <> show e
        Right names -> do
            let classify name = do
                    isDir <- doesDirectoryExist (path </> name)
                    modified <- try $ getModificationTime (path </> name)
                    pure
                        ( either (\(_ :: IOException) -> Nothing) Just modified
                        , (isDir, T.pack name)
                        )
            entries <- mapM classify names
            pure $
                Right
                    ( path
                    , map snd
                        $ sortOn (\(t, (_, n)) -> (Down t, n))
                        $ filter (shown . snd) entries
                    )
  where
    shown (isDir, name) =
        not (T.isPrefixOf "." name) && (isDir || isImage name)

-- | Whether a file name is an image the QR reader can open.
isImage :: Text -> Bool
isImage name =
    map toLower (takeExtension $ T.unpack name)
        `elem` [".png", ".jpg", ".jpeg", ".gif", ".bmp", ".webp"]

module Euicc.Lpac.Command
    ( Command (..)
    , commandArgs
    , lpacEnvironment
    ) where

-- \|
-- Module      : Euicc.Lpac.Command
-- Description : The lpac invocations this program makes
-- Copyright   : (c) Paolo Veronelli, 2026
-- License     : Apache-2.0
--
-- The closed set of @lpac@ commands the UI can issue. There is no
-- constructor for deleting or disabling a profile, nor for removing a
-- notification without sending it: those operations cannot be
-- expressed.

import Data.Text (Text)
import Data.Text qualified as T
import Euicc.ActivationCode (DownloadTarget (..), revealSecret)

-- | An @lpac@ invocation.
data Command
    = ReadChipInfo
    | ListProfiles
    | -- | enable the profile with this ICCID
      EnableProfile Text
    | ListNotifications
    | {- | send these notifications and drop each one the server
      accepted
      -}
      ProcessNotifications [Int]
    | DownloadProfile DownloadTarget
    deriving stock (Eq, Show)

-- | The argument vector passed to @lpac@.
commandArgs :: Command -> [String]
commandArgs = \case
    ReadChipInfo -> ["chip", "info"]
    ListProfiles -> ["profile", "list"]
    EnableProfile iccid -> ["profile", "enable", T.unpack iccid]
    ListNotifications -> ["notification", "list"]
    ProcessNotifications seqs ->
        ["notification", "process", "-r"] <> map show seqs
    DownloadProfile DownloadTarget{..} ->
        [ "profile"
        , "download"
        , "-s"
        , T.unpack targetSmdp
        , "-m"
        , T.unpack $ revealSecret targetMatchingId
        ]

{- | The environment for @lpac@: the given one with @LPAC_APDU@ forced
to @pcsc@.
-}
lpacEnvironment :: [(String, String)] -> [(String, String)]
lpacEnvironment env =
    ("LPAC_APDU", "pcsc") : filter ((/= "LPAC_APDU") . fst) env

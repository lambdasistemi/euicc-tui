module Euicc.Lpac.Output
    ( -- * Raw process output
      RawOutput (..)

      -- * Card data
    , ChipInfo (..)
    , Profile (..)
    , ProfileState (..)
    , Notification (..)
    , profileLabel

      -- * Failures
    , LpacFailure (..)
    , describeFailure

      -- * Decoding
    , decodeResult
    , parseChipInfo
    , parseProfiles
    , parseNotifications
    , parseDone
    ) where

-- \|
-- Module      : Euicc.Lpac.Output
-- Description : Pure decoding of lpac JSON output
-- Copyright   : (c) Paolo Veronelli, 2026
-- License     : Apache-2.0
--
-- @lpac@ prints one JSON object per line on stdout. Lines of type
-- @progress@ report intermediate steps; the last line of type @lpa@ is
-- the result, with @code@ 0 on success. PC/SC and APDU driver failures
-- are printed on stderr instead, often with no @lpa@ line at all.
--
-- This module turns a captured run into either the decoded payload or
-- an 'LpacFailure' that says what the operator can do about it.

import Control.Applicative ((<|>))
import Control.Monad (mfilter, void)
import Data.Aeson
    ( FromJSON (..)
    , Value (..)
    , decodeStrict
    , encode
    , withArray
    , withObject
    , (.:)
    , (.:?)
    )
import Data.Aeson.Types (Parser, parseEither)
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as BL
import Data.Foldable (find, toList)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import System.Exit (ExitCode (..))

-- | Everything a finished @lpac@ run produced.
data RawOutput = RawOutput
    { rawExit :: ExitCode
    , rawStdout :: ByteString
    , rawStderr :: ByteString
    }
    deriving stock (Eq, Show)

-- | The parts of @lpac chip info@ the UI shows.
data ChipInfo = ChipInfo
    { chipEid :: Text
    -- ^ the card's EID
    , chipFreeMemory :: Maybe Integer
    -- ^ free non-volatile memory in bytes, when reported
    }
    deriving stock (Eq, Show)

-- | Whether a profile is the active one.
data ProfileState
    = Enabled
    | Disabled
    | OtherState Text
    deriving stock (Eq, Show)

-- | One installed profile, as listed by @lpac profile list@.
data Profile = Profile
    { profileIccid :: Text
    , profileAid :: Text
    , profileState :: ProfileState
    , profileNickname :: Maybe Text
    , profileProvider :: Maybe Text
    , profileName :: Maybe Text
    }
    deriving stock (Eq, Show)

-- | One pending notification, as listed by @lpac notification list@.
data Notification = Notification
    { notificationSeq :: Int
    , notificationOperation :: Text
    , notificationAddress :: Maybe Text
    , notificationIccid :: Maybe Text
    }
    deriving stock (Eq, Show)

-- | Why an @lpac@ run did not produce a usable result.
data LpacFailure
    = -- | PC/SC sees no reader (@8010002E@)
      NoReader
    | -- | a reader is present but no card answers
      NoCard
    | -- | pcscd refused the client via polkit (@8010006A@)
      PolkitDenied
    | -- | the reader's USB device is not accessible
      UsbAccessDenied
    | -- | lpac used a modem backend instead of PC/SC
      WrongApduBackend
    | -- | a QR image could not be decoded into an activation code
      QrDecode Text
    | -- | a directory could not be listed for the picker
      DirFailure Text
    | -- | lpac reported an error: failing function and detail
      LpacError Text Text
    | -- | output that could not be understood, with context
      UnexpectedOutput Text
    deriving stock (Eq, Show)

-- | The name shown for a profile: nickname, else name, else ICCID.
profileLabel :: Profile -> Text
profileLabel Profile{..} =
    fromMaybe profileIccid $
        nonEmpty profileNickname <|> nonEmpty profileName
  where
    nonEmpty = mfilter (not . T.null . T.strip)

-- | A one-line, actionable explanation of a failure.
describeFailure :: LpacFailure -> Text
describeFailure = \case
    NoReader ->
        "No smart-card reader is visible to pcscd (8010002E). \
        \Plug the reader in and check that pcscd is running."
    NoCard ->
        "The reader has no card, or the card does not answer. \
        \Insert the eUICC card and try again."
    PolkitDenied ->
        "pcscd refused access (8010006A): its polkit policy only \
        \admits the active local session. Run at the machine's own \
        \desk, or via sudo."
    UsbAccessDenied ->
        "The reader's USB device is not accessible \
        \(LIBUSB_ERROR_ACCESS): it was probably plugged in before \
        \its udev rules applied. Unplug and re-plug the reader."
    WrongApduBackend ->
        "lpac tried a modem backend instead of the smart-card \
        \reader. It must run with LPAC_APDU=pcsc."
    QrDecode msg -> "The QR image could not be used: " <> msg
    DirFailure msg -> "The directory could not be read: " <> msg
    LpacError function detail
        | T.null (T.strip detail) -> "lpac failed in " <> function <> "."
        | otherwise -> "lpac failed in " <> function <> ": " <> detail
    UnexpectedOutput msg -> "Unexpected output from lpac: " <> msg

-- | One line of lpac's stdout.
data Line = Line
    { lineType :: Text
    , lineCode :: Int
    , lineMessage :: Text
    , lineData :: Value
    }

instance FromJSON Line where
    parseJSON = withObject "lpac line" $ \o -> do
        lineType <- o .: "type"
        payload <- o .: "payload"
        lineCode <- payload .: "code"
        lineMessage <- fromMaybe "" <$> payload .:? "message"
        lineData <- fromMaybe Null <$> payload .:? "data"
        pure Line{..}

-- | Markers of known environment failures, most specific first.
knownFailures :: [(Text, LpacFailure)]
knownFailures =
    [ ("LIBUSB_ERROR_ACCESS", UsbAccessDenied)
    , ("8010006A", PolkitDenied)
    , ("8010002E", NoReader)
    , ("8010000C", NoCard)
    , ("80100069", NoCard)
    , ("/dev/cdc-wdm", WrongApduBackend)
    ]

-- | The @data@ of a successful run, or the classified failure.
decodeResult :: RawOutput -> Either LpacFailure Value
decodeResult RawOutput{..} = case lastResult of
    Just Line{lineCode = 0, lineData} -> Right lineData
    result -> Left $ case find matches knownFailures of
        Just (_, failure) -> failure
        Nothing -> case result of
            Just Line{lineMessage, lineData} ->
                LpacError lineMessage $ detailText lineData
            Nothing ->
                UnexpectedOutput $
                    "no result line (" <> exitText <> ")" <> stderrText
  where
    everything = decodeLenient rawStdout <> "\n" <> decodeLenient rawStderr
    matches (marker, _) = marker `T.isInfixOf` everything
    lastResult =
        find ((== "lpa") . lineType)
            $ reverse
            $ mapMaybe (decodeStrict . BS8.strip)
            $ BS8.lines rawStdout
    exitText = case rawExit of
        ExitSuccess -> "exit 0"
        ExitFailure n -> "exit " <> T.pack (show n)
    stderrText = case T.strip $ decodeLenient rawStderr of
        "" -> ""
        err -> ": " <> err
    detailText = \case
        String t -> t
        Null -> ""
        v -> decodeLenient $ BL.toStrict $ encode v

decodeLenient :: ByteString -> Text
decodeLenient = decodeUtf8With lenientDecode

-- | Decode the data of a successful run with the given parser.
parseWith :: (Value -> Parser a) -> RawOutput -> Either LpacFailure a
parseWith p out = do
    v <- decodeResult out
    first (UnexpectedOutput . T.pack) $ parseEither p v

-- | Decode @lpac chip info@.
parseChipInfo :: RawOutput -> Either LpacFailure ChipInfo
parseChipInfo = parseWith $ withObject "chip info" $ \o -> do
    chipEid <- o .: "eidValue"
    info <- o .:? "EUICCInfo2"
    resource <- maybe (pure Nothing) (.:? "extCardResource") info
    chipFreeMemory <-
        maybe (pure Nothing) (.:? "freeNonVolatileMemory") resource
    pure ChipInfo{..}

-- | Decode @lpac profile list@.
parseProfiles :: RawOutput -> Either LpacFailure [Profile]
parseProfiles = parseWith $ withArray "profiles" $ \a ->
    traverse profile $ toList a
  where
    profile = withObject "profile" $ \o -> do
        profileIccid <- o .: "iccid"
        profileAid <- fromMaybe "" <$> o .:? "isdpAid"
        profileState <- state <$> o .:? "profileState"
        profileNickname <- o .:? "profileNickname"
        profileProvider <- o .:? "serviceProviderName"
        profileName <- o .:? "profileName"
        pure Profile{..}
    state = \case
        Just "enabled" -> Enabled
        Just "disabled" -> Disabled
        Just other -> OtherState other
        Nothing -> OtherState "unknown"

-- | Decode @lpac notification list@.
parseNotifications
    :: RawOutput -> Either LpacFailure [Notification]
parseNotifications = parseWith $ withArray "notifications" $ \a ->
    traverse notification $ toList a
  where
    notification = withObject "notification" $ \o -> do
        notificationSeq <- o .: "seqNumber"
        notificationOperation <-
            fromMaybe "unknown" <$> o .:? "profileManagementOperation"
        notificationAddress <- o .:? "notificationAddress"
        notificationIccid <- o .:? "iccid"
        pure Notification{..}

-- | Decode a run whose only interesting output is success.
parseDone :: RawOutput -> Either LpacFailure ()
parseDone = void . decodeResult

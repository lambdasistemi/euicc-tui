module Euicc.Job
    ( -- * Running lpac
      LpacRunner (..)

      -- * Jobs
    , Job (..)
    , jobLabel
    , Snapshot (..)
    , JobResult (..)
    , jobResult
    , runJob
    , loadSnapshot
    ) where

-- \|
-- Module      : Euicc.Job
-- Description : What the UI asks for, and how it is done with lpac
-- Copyright   : (c) Paolo Veronelli, 2026
-- License     : Apache-2.0
--
-- A 'Job' is one user request. Jobs that touch the card run through
-- an 'LpacRunner' and then reload the card state, so the UI always
-- shows what the card says after the action, whether the action
-- succeeded or not. 'DecodeQr' reads a QR image instead and leaves
-- the card state alone. The runner is the only point of contact with
-- the hardware, so tests replace it with recorded outputs.

import Control.Exception (SomeException, displayException, try)
import Data.Bifunctor (first)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Euicc.ActivationCode (DownloadTarget (..), Secret, redact)
import Euicc.Lpac.Command (Command (..))
import Euicc.Lpac.Output
    ( ChipInfo
    , LpacFailure (..)
    , Notification
    , Profile (..)
    , RawOutput (..)
    , parseChipInfo
    , parseDone
    , parseNotifications
    , parseProfiles
    , profileLabel
    )
import Euicc.Qr (decodeQrFile)
import System.Exit (ExitCode (..))

-- | How to run one @lpac@ command.
newtype LpacRunner = LpacRunner
    { runLpac :: Command -> IO RawOutput
    }

-- | A user request.
data Job
    = -- | reload chip info, profiles and notifications
      Refresh
    | -- | enable the given profile
      Enable Profile
    | -- | give the given profile a nickname
      Nickname Profile Text
    | -- | send these notifications
      SendNotifications [Int]
    | -- | download a profile, with a confirmation code when needed
      Download DownloadTarget (Maybe Secret)
    | -- | read an activation code from a QR image file
      DecodeQr FilePath
    deriving stock (Eq, Show)

-- | Everything the UI shows about the card.
data Snapshot = Snapshot
    { snapChip :: ChipInfo
    , snapProfiles :: [Profile]
    , snapNotifications :: [Notification]
    }
    deriving stock (Eq, Show)

-- | The outcome of a job and the card state read after it.
data JobResult = JobResult
    { resultJob :: Job
    -- ^ the job this result belongs to
    , resultOutcome :: Either LpacFailure Text
    -- ^ what happened to the requested action
    , resultSnapshot :: Maybe (Either LpacFailure Snapshot)
    -- ^ the card as read afterwards; 'Nothing' when the job did not
    -- touch the card
    , resultQr :: Maybe DownloadTarget
    -- ^ the activation code a 'DecodeQr' job read, if any
    }
    deriving stock (Eq, Show)

{- | A result with no QR payload, for tests and callers that only
need the outcome and the card state.
-}
jobResult
    :: Job
    -> Either LpacFailure Text
    -> Maybe (Either LpacFailure Snapshot)
    -> JobResult
jobResult job outcome snapshot =
    JobResult
        { resultJob = job
        , resultOutcome = outcome
        , resultSnapshot = snapshot
        , resultQr = Nothing
        }

-- | A short description of a running job, for the busy indicator.
jobLabel :: Job -> Text
jobLabel = \case
    Refresh -> "reading the card"
    Enable p -> "enabling " <> profileLabel p
    Nickname p _ -> "naming " <> profileLabel p
    SendNotifications [_] -> "sending 1 notification"
    SendNotifications ns ->
        "sending " <> T.pack (show $ length ns) <> " notifications"
    Download DownloadTarget{targetSmdp} _ ->
        "downloading from " <> targetSmdp
    DecodeQr _ -> "reading the QR image"

{- | Run one command, turning any exception into output so that a
failure is always reported, never thrown.
-}
runSafely :: LpacRunner -> Command -> IO RawOutput
runSafely runner command = do
    r <- try $ runLpac runner command
    pure $ case r of
        Right out -> out
        Left (e :: SomeException) ->
            RawOutput
                { rawExit = ExitFailure 1
                , rawStdout = ""
                , rawStderr = encodeUtf8 $ T.pack $ displayException e
                }

-- | Read chip info, profiles and notifications.
loadSnapshot :: LpacRunner -> IO (Either LpacFailure Snapshot)
loadSnapshot runner = do
    chip <- parseChipInfo <$> runSafely runner ReadChipInfo
    profiles <- parseProfiles <$> runSafely runner ListProfiles
    notifications <-
        parseNotifications <$> runSafely runner ListNotifications
    pure $ Snapshot <$> chip <*> profiles <*> notifications

-- | Perform a job, then reload the card state.
runJob :: LpacRunner -> Job -> IO JobResult
runJob runner job = case job of
    DecodeQr path -> do
        decoded <- decodeQrFile path
        pure
            JobResult
                { resultJob = job
                , resultOutcome = (const "QR code read.") <$> decoded
                , resultSnapshot = Nothing
                , resultQr = either (const Nothing) Just decoded
                }
    _ -> do
        outcome <- act
        snapshot <- Just <$> loadSnapshot runner
        pure
            JobResult
                { resultJob = job
                , resultOutcome = case job of
                    Refresh ->
                        maybe (Right "Card read.") (fmap $ const "Card read.")
                            snapshot
                    _ -> outcome
                , resultSnapshot = snapshot
                , resultQr = Nothing
                }
  where
    done message command =
        (message <$) . parseDone <$> runSafely runner command
    act = case job of
        Refresh -> pure $ Right ""
        Enable p ->
            done ("Enabled " <> profileLabel p <> ".")
                $ EnableProfile
                $ profileIccid p
        Nickname p nickname ->
            done ("Named " <> profileLabel p <> ".")
                $ NicknameProfile (profileIccid p) nickname
        SendNotifications seqs ->
            done "Notifications sent." $ ProcessNotifications seqs
        Download target@DownloadTarget{targetMatchingId} confirmation ->
            first (redactFailure targetMatchingId)
                <$> done
                    "Profile downloaded."
                    (DownloadProfile target confirmation)
        DecodeQr _ -> pure $ Right ""

-- | Remove a secret from every text a failure carries.
redactFailure :: Secret -> LpacFailure -> LpacFailure
redactFailure secret = \case
    LpacError function detail ->
        LpacError (redact secret function) (redact secret detail)
    UnexpectedOutput msg -> UnexpectedOutput $ redact secret msg
    other -> other

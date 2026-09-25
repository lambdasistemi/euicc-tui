module Euicc.Job
    ( -- * Running lpac
      LpacRunner (..)

      -- * Jobs
    , Job (..)
    , jobLabel
    , Snapshot (..)
    , JobResult (..)
    , runJob
    , loadSnapshot
    ) where

-- \|
-- Module      : Euicc.Job
-- Description : What the UI asks for, and how it is done with lpac
-- Copyright   : (c) Paolo Veronelli, 2026
-- License     : Apache-2.0
--
-- A 'Job' is one user request. 'runJob' performs it through an
-- 'LpacRunner' and then reloads the card state, so the UI always shows
-- what the card says after the action, whether the action succeeded or
-- not. The runner is the only point of contact with the hardware, so
-- tests replace it with recorded outputs.

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
    | -- | send these notifications
      SendNotifications [Int]
    | -- | download a profile, with a confirmation code when needed
      Download DownloadTarget (Maybe Secret)
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
    { resultOutcome :: Either LpacFailure Text
    -- ^ what happened to the requested action
    , resultSnapshot :: Either LpacFailure Snapshot
    -- ^ the card as read afterwards
    }
    deriving stock (Eq, Show)

-- | A short description of a running job, for the busy indicator.
jobLabel :: Job -> Text
jobLabel = \case
    Refresh -> "reading the card"
    Enable p -> "enabling " <> profileLabel p
    SendNotifications [_] -> "sending 1 notification"
    SendNotifications ns ->
        "sending " <> T.pack (show $ length ns) <> " notifications"
    Download DownloadTarget{targetSmdp} _ ->
        "downloading from " <> targetSmdp

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
runJob runner job = do
    outcome <- act
    snapshot <- loadSnapshot runner
    pure
        JobResult
            { resultOutcome = case job of
                Refresh -> "Card read." <$ snapshot
                _ -> outcome
            , resultSnapshot = snapshot
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
        SendNotifications seqs ->
            done "Notifications sent." $ ProcessNotifications seqs
        Download target@DownloadTarget{targetMatchingId} confirmation ->
            first (redactFailure targetMatchingId)
                <$> done
                    "Profile downloaded."
                    (DownloadProfile target confirmation)

-- | Remove a secret from every text a failure carries.
redactFailure :: Secret -> LpacFailure -> LpacFailure
redactFailure secret = \case
    LpacError function detail ->
        LpacError (redact secret function) (redact secret detail)
    UnexpectedOutput msg -> UnexpectedOutput $ redact secret msg
    other -> other

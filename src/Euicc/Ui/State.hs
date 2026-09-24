module Euicc.Ui.State
    ( -- * State
      State (..)
    , View (..)
    , Form (..)
    , Field (..)
    , Status (..)
    , emptyForm

      -- * Transitions
    , Step (..)
    , start
    , handleKey
    , finishJob

      -- * Queries
    , selectedProfile
    , selectedNotification
    , codeDisplay
    , smdpDisplay
    ) where

-- \|
-- Module      : Euicc.Ui.State
-- Description : The UI as a pure state machine
-- Copyright   : (c) Paolo Veronelli, 2026
-- License     : Apache-2.0
--
-- Key presses turn into a new 'State' and, at most, one 'Job' to run.
-- The job's 'JobResult' comes back through 'finishJob'. Nothing here
-- touches the terminal or the card, so every interaction can be tested
-- without hardware.
--
-- While a job runs the state is busy and no further job is started.
-- There is no key that deletes or disables a profile.

import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Euicc.ActivationCode (mask, resolveDownloadInput)
import Euicc.Job
    ( Job (..)
    , JobResult (..)
    , Snapshot (..)
    , jobLabel
    )
import Euicc.Lpac.Output
    ( LpacFailure
    , Notification (..)
    , Profile (..)
    , ProfileState (..)
    , describeFailure
    , profileLabel
    )
import Graphics.Vty (Key (..), Modifier (..))

-- | Which screen is shown.
data View
    = ProfilesView
    | NotificationsView
    | DownloadView
    deriving stock (Eq, Show)

-- | The two fields of the download form.
data Field
    = SmdpField
    | CodeField
    deriving stock (Eq, Show)

-- | The download form.
data Form = Form
    { formSmdp :: Text
    , formCode :: Text
    , formFocus :: Field
    }
    deriving stock (Eq)

-- | The code is never shown, not even inside the address field.
instance Show Form where
    show form@Form{formFocus} =
        "Form {formSmdp = "
            <> show (smdpDisplay form)
            <> ", formCode = <redacted>, formFocus = "
            <> show formFocus
            <> "}"

-- | A form with nothing typed.
emptyForm :: Form
emptyForm = Form{formSmdp = "", formCode = "", formFocus = SmdpField}

-- | The message line.
data Status
    = Info Text
    | Failure Text
    deriving stock (Eq, Show)

-- | The whole UI state.
data State = State
    { stView :: View
    , stCard :: Maybe (Either LpacFailure Snapshot)
    -- ^ the last card read, if any has completed
    , stProfileCursor :: Int
    , stNotificationCursor :: Int
    , stForm :: Form
    , stConfirm :: Maybe Profile
    -- ^ a profile waiting for y/n before being enabled
    , stBusy :: Maybe Job
    -- ^ the job in flight
    , stStatus :: Maybe Status
    }
    deriving stock (Eq, Show)

-- | The result of a key press.
data Step
    = -- | the new state and the job to start, if any
      Continue State (Maybe Job)
    | -- | leave the program
      Halt
    deriving stock (Eq, Show)

-- | The initial state, already busy with the first card read.
start :: (State, Job)
start =
    ( State
        { stView = ProfilesView
        , stCard = Nothing
        , stProfileCursor = 0
        , stNotificationCursor = 0
        , stForm = emptyForm
        , stConfirm = Nothing
        , stBusy = Just Refresh
        , stStatus = Nothing
        }
    , Refresh
    )

-- | React to a key press.
handleKey :: Key -> [Modifier] -> State -> Step
handleKey key mods s
    | key == KChar 'c' && MCtrl `elem` mods = Halt
    | Just p <- stConfirm s = confirming p
    | otherwise = case stView s of
        ProfilesView -> profiles
        NotificationsView -> notifications
        DownloadView -> download
  where
    continue s' = Continue s' Nothing
    confirming p = case key of
        KChar 'y' -> launch (Enable p) s{stConfirm = Nothing}
        _ ->
            continue s{stConfirm = Nothing, stStatus = Just $ Info "Cancelled."}
    profiles = case key of
        KChar 'q' -> Halt
        KUp -> continue $ moveProfile (-1)
        KChar 'k' -> continue $ moveProfile (-1)
        KDown -> continue $ moveProfile 1
        KChar 'j' -> continue $ moveProfile 1
        KChar 'e' -> askEnable
        KEnter -> askEnable
        KChar 'r' -> launch Refresh s
        KChar 'n' -> switchTo NotificationsView
        KChar 'd' -> switchTo DownloadView
        _ -> continue s
    notifications = case key of
        KChar 'q' -> Halt
        KUp -> continue $ moveNotification (-1)
        KChar 'k' -> continue $ moveNotification (-1)
        KDown -> continue $ moveNotification 1
        KChar 'j' -> continue $ moveNotification 1
        KChar 's' ->
            maybe
                (continue s{stStatus = Just $ Info "Nothing to send."})
                (\n -> launch (SendNotifications [notificationSeq n]) s)
                $ selectedNotification s
        KChar 'a' -> case map notificationSeq $ notificationsOf s of
            [] -> continue s{stStatus = Just $ Info "Nothing to send."}
            seqs -> launch (SendNotifications seqs) s
        KChar 'r' -> launch Refresh s
        KChar 'p' -> continue s{stView = ProfilesView}
        KEsc -> continue s{stView = ProfilesView}
        KChar 'd' -> switchTo DownloadView
        _ -> continue s
    download = case key of
        KEsc ->
            continue
                s{stView = ProfilesView, stForm = emptyForm}
        KEnter -> submit
        KChar '\t' -> continue $ onForm switchField
        KBackTab -> continue $ onForm switchField
        KUp -> continue $ onForm switchField
        KDown -> continue $ onForm switchField
        KBS -> continue $ onForm $ editField $ T.dropEnd 1
        KChar c -> continue $ onForm $ editField (`T.snoc` c)
        _ -> continue s
    askEnable = case selectedProfile s of
        Nothing -> continue s
        Just p
            | profileState p == Enabled ->
                continue
                    s
                        { stStatus =
                            Just $ Info $ profileLabel p <> " is already enabled."
                        }
            | otherwise -> continue s{stConfirm = Just p}
    submit =
        let Form{formSmdp, formCode} = stForm s
        in  case resolveDownloadInput formSmdp formCode of
                Left err -> continue s{stStatus = Just $ Failure err}
                Right target ->
                    launch
                        (Download target)
                        s{stForm = emptyForm, stView = ProfilesView}
    onForm f = s{stForm = f $ stForm s}
    switchTo view = case snapshotOf s of
        Just _ -> continue s{stView = view}
        Nothing -> continue s
    moveProfile d =
        s
            { stProfileCursor =
                clamp (length $ profilesOf s) $ stProfileCursor s + d
            }
    moveNotification d =
        s
            { stNotificationCursor =
                clamp (length $ notificationsOf s) $
                    stNotificationCursor s + d
            }
    launch job s'
        | Just running <- stBusy s' =
            continue
                s'
                    { stStatus =
                        Just $ Info $ "Busy " <> jobLabel running <> "."
                    }
        | otherwise = Continue s'{stBusy = Just job} $ Just job

switchField :: Form -> Form
switchField f = f{formFocus = other $ formFocus f}
  where
    other SmdpField = CodeField
    other CodeField = SmdpField

editField :: (Text -> Text) -> Form -> Form
editField edit f = case formFocus f of
    SmdpField -> f{formSmdp = edit $ formSmdp f}
    CodeField -> f{formCode = edit $ formCode f}

-- | Keep a cursor within a list of the given length.
clamp :: Int -> Int -> Int
clamp n i = max 0 $ min (n - 1) i

snapshotOf :: State -> Maybe Snapshot
snapshotOf s = case stCard s of
    Just (Right snap) -> Just snap
    _ -> Nothing

profilesOf :: State -> [Profile]
profilesOf = maybe [] snapProfiles . snapshotOf

notificationsOf :: State -> [Notification]
notificationsOf = maybe [] snapNotifications . snapshotOf

-- | Record a finished job.
finishJob :: JobResult -> State -> State
finishJob JobResult{..} s =
    let s' =
            s
                { stBusy = Nothing
                , stCard = Just resultSnapshot
                , stStatus = case (resultOutcome, resultSnapshot) of
                    (Left f, Left g) | f == g -> Nothing
                    (Left f, _) -> Just $ Failure $ describeFailure f
                    (Right msg, _) -> Just $ Info msg
                }
    in  s'
            { stProfileCursor =
                clamp (length $ profilesOf s') $ stProfileCursor s'
            , stNotificationCursor =
                clamp (length $ notificationsOf s') $
                    stNotificationCursor s'
            }

-- | The profile under the cursor.
selectedProfile :: State -> Maybe Profile
selectedProfile s = atIndex (stProfileCursor s) $ profilesOf s

-- | The notification under the cursor.
selectedNotification :: State -> Maybe Notification
selectedNotification s =
    atIndex (stNotificationCursor s) $ notificationsOf s

atIndex :: Int -> [a] -> Maybe a
atIndex i = listToMaybe . drop i

-- | What the activation code field shows.
codeDisplay :: Form -> Text
codeDisplay = mask . formCode

{- | What the SM-DP+ address field shows: the text as typed, except
that the code inside a pasted @LPA:@ string is masked.
-}
smdpDisplay :: Form -> Text
smdpDisplay Form{formSmdp}
    | "LPA:" `T.isPrefixOf` T.stripStart formSmdp =
        T.intercalate "$"
            $ zipWith maskFrom [0 :: Int ..]
            $ T.splitOn "$" formSmdp
    | otherwise = formSmdp
  where
    maskFrom i segment
        | i >= 2 = mask segment
        | otherwise = segment

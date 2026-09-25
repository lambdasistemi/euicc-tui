module Euicc.Ui.State
    ( -- * State
      State (..)
    , View (..)
    , Form (..)
    , Field (..)
    , Status (..)
    , Wizard (..)
    , WizardPhase (..)
    , confirmDisplay
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
import Control.Applicative ((<|>))

import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Euicc.ActivationCode
    ( DownloadTarget (..)
    , mask
    , mkSecret
    , resolveDownloadInput
    , revealSecret
    )
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
    | WizardView
    deriving stock (Eq, Show)

-- | The fields of the download form, in tab order.
data Field
    = QrField
    | SmdpField
    | CodeField
    deriving stock (Eq, Show)

-- | The download form.
data Form = Form
    { formQr :: Text
    -- ^ path to a QR image, read on Enter
    , formSmdp :: Text
    , formCode :: Text
    , formFocus :: Field
    }
    deriving stock (Eq)

-- | The code is never shown, not even inside the address field.
instance Show Form where
    show form@Form{formFocus} =
        "Form {formQr = "
            <> show (formQr form)
            <> ", formSmdp = "
            <> show (smdpDisplay form)
            <> ", formCode = <redacted>, formFocus = "
            <> show formFocus
            <> "}"

-- | A form with nothing typed.
emptyForm :: Form
emptyForm =
    Form
        { formQr = ""
        , formSmdp = ""
        , formCode = ""
        , formFocus = SmdpField
        }

-- | The message line.
data Status
    = Info Text
    | Failure Text
    deriving stock (Eq, Show)

-- | Where a guided install stands.
data WizardPhase
    = -- | QR image path, SM-DP+ address and activation code
      WzSource
    | -- | the activation code asks for a confirmation code
      WzConfirm
    | -- | optional nickname for the plan just downloaded
      WzNickname
    | -- | closing instruction, everything worked
      WzDone
    deriving stock (Eq, Show)

-- | The state of a guided install.
data Wizard = Wizard
    { wzPhase :: WizardPhase
    , wzKnownIccids :: [Text]
    {- ^ the profiles that existed before the download; the new plan
    is the one that appears besides them
    -}
    , wzNew :: Maybe Profile
    -- ^ the plan the wizard just downloaded
    , wzNicknameInput :: Text
    , wzConfirmInput :: Text
    -- ^ the confirmation code while it is typed; cleared on submit
    }
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
    , stNicknameEdit :: Maybe (Profile, Text)
    -- ^ a profile waiting for a nickname to be typed
    , stWizard :: Maybe Wizard
    -- ^ a guided install in progress
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
        , stNicknameEdit = Nothing
        , stWizard = Nothing
        , stBusy = Just Refresh
        , stStatus = Nothing
        }
    , Refresh
    )

-- | React to a key press.
handleKey :: Key -> [Modifier] -> State -> Step
handleKey key mods s
    | key == KChar 'c' && MCtrl `elem` mods = Halt
    | Just (p, t) <- stNicknameEdit s = nicknaming p t
    | Just p <- stConfirm s = confirming p
    | otherwise = case stView s of
        ProfilesView -> profiles
        NotificationsView -> notifications
        DownloadView -> download
        WizardView -> wizard
  where
    continue s' = Continue s' Nothing
    nicknaming p t = case key of
        KEnter
            | T.null (T.strip t) -> cancelNickname
            | otherwise ->
                launch (Nickname p (T.strip t)) s{stNicknameEdit = Nothing}
        KEsc -> cancelNickname
        KBS -> continue s{stNicknameEdit = Just (p, T.dropEnd 1 t)}
        KChar c -> continue s{stNicknameEdit = Just (p, T.snoc t c)}
        _ -> continue s
      where
        cancelNickname = continue s{stNicknameEdit = Nothing}
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
        KChar 'g' -> openWizard
        KChar 'm' -> case selectedProfile s of
            Nothing -> continue s
            Just p ->
                continue
                    s
                        { stNicknameEdit =
                            Just (p, fromMaybe "" $ profileNickname p)
                        }
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
        KEnter -> formEnter
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
    wizard = case wzPhase $ wizardOf s of
        WzSource -> case key of
            KEsc -> closeWizard s
            KChar '\t' -> continue $ onForm switchField
            KBackTab -> continue $ onForm switchField
            KUp -> continue $ onForm switchField
            KDown -> continue $ onForm switchField
            KBS -> continue $ onForm $ editField $ T.dropEnd 1
            KChar c -> continue $ onForm $ editField (`T.snoc` c)
            KEnter -> formEnter
            _ -> continue s
        WzConfirm -> case key of
            KEsc -> closeWizard s
            KBS -> continue s{stWizard = editConfirm <$> stWizard s}
            KChar c ->
                continue s{stWizard = (`snocConfirm` c) <$> stWizard s}
            KEnter
                | T.null (T.strip $ wzConfirmInput $ wizardOf s) ->
                    continue
                        s{stStatus = Just $ Info "Type the confirmation code."}
                | otherwise ->
                    case resolveDownloadInput
                        (formSmdp $ stForm s)
                        (formCode $ stForm s) of
                        Left err -> continue s{stStatus = Just $ Failure err}
                        Right target ->
                            launch
                                ( Download target
                                    $ Just
                                    $ mkSecret
                                    $ T.strip
                                    $ wzConfirmInput
                                    $ wizardOf s
                                )
                                s
                                    { stForm = emptyForm
                                    , stWizard =
                                        (\w -> w{wzConfirmInput = ""})
                                            <$> stWizard s
                                    }
            _ -> continue s
        WzNickname -> case key of
            KEnter
                | T.null (T.strip $ wzNicknameInput $ wizardOf s) -> toEnableAsk
                | otherwise -> case wzNew $ wizardOf s of
                    Nothing -> toEnableAsk
                    Just p ->
                        launch
                            (Nickname p (T.strip $ wzNicknameInput $ wizardOf s))
                            s
                                { stWizard =
                                    (\w -> w{wzNicknameInput = ""})
                                        <$> stWizard s
                                }
            KEsc -> toEnableAsk
            KBS ->
                continue s{stWizard = editNicknameInput <$> stWizard s}
            KChar c ->
                continue s{stWizard = (`snocNickname` c) <$> stWizard s}
            _ -> continue s
        WzDone -> case key of
            KEsc -> closeWizard s
            KEnter -> closeWizard s
            _ -> continue s
    formEnter = case formFocus $ stForm s of
        QrField
            | T.null (T.strip $ formQr $ stForm s) ->
                continue $ onForm $ focus SmdpField
            | otherwise ->
                launch
                    (DecodeQr $ T.unpack $ T.strip $ formQr $ stForm s)
                    s
        SmdpField -> continue $ onForm focusCode
        CodeField -> submit
    openWizard = case snapshotOf s of
        Nothing ->
            continue s{stStatus = Just $ Info "Read the card first (r)."}
        Just snap ->
            continue
                s
                    { stView = WizardView
                    , stForm = emptyForm{formFocus = QrField}
                    , stWizard =
                        Just
                            Wizard
                                { wzPhase = WzSource
                                , wzKnownIccids =
                                    map profileIccid $ snapProfiles snap
                                , wzNew = Nothing
                                , wzNicknameInput = ""
                                , wzConfirmInput = ""
                                }
                    }
    closeWizard st =
        Continue
            st
                { stView = ProfilesView
                , stForm = emptyForm
                , stWizard = Nothing
                }
            Nothing
    wizardOf st = case stWizard st of
        Just w -> w
        Nothing -> Wizard WzSource [] Nothing "" ""
    toEnableAsk = case wzNew $ wizardOf s of
        Nothing -> closeWizard s
        Just p -> continue s{stConfirm = Just p}
    submit =
        let Form{formSmdp, formCode} = stForm s
        in  case resolveDownloadInput formSmdp formCode of
                Left err -> continue s{stStatus = Just $ Failure err}
                Right target
                    | targetConfirmationRequired target
                    , Just w <- stWizard s ->
                        continue
                            s
                                { stWizard = Just w{wzPhase = WzConfirm}
                                , stStatus =
                                    Just $
                                        Info
                                            "This code asks for a \
                                            \confirmation code."
                                }
                    | targetConfirmationRequired target ->
                        continue
                            s
                                { stStatus =
                                    Just $
                                        Failure
                                            "This code asks for a \
                                            \confirmation code; use the \
                                            \guided install (g)."
                                }
                    | Just _ <- stWizard s ->
                        launch (Download target Nothing) s{stForm = emptyForm}
                    | otherwise ->
                        launch
                            (Download target Nothing)
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
switchField f = f{formFocus = next $ formFocus f}
  where
    next QrField = SmdpField
    next SmdpField = CodeField
    next CodeField = QrField

editField :: (Text -> Text) -> Form -> Form
editField edit f = case formFocus f of
    QrField -> f{formQr = edit $ formQr f}
    SmdpField -> f{formSmdp = edit $ formSmdp f}
    CodeField -> f{formCode = edit $ formCode f}

{- | What the confirmation-code field of the wizard shows: one @*@ per
character.
-}
confirmDisplay :: Wizard -> Text
confirmDisplay = mask . wzConfirmInput

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

{- | Record a finished job, starting the next job of a guided
install when there is one.
-}
finishJob :: JobResult -> State -> (State, Maybe Job)
finishJob JobResult{..} s =
    let filled = case resultQr of
            Just target -> fillFrom target s
            Nothing -> s
        s1 =
            filled
                { stBusy = Nothing
                , stCard = resultSnapshot <|> stCard filled
                , stStatus = case (resultOutcome, resultSnapshot) of
                    (Left f, Just (Left g)) | f == g -> Nothing
                    (Left f, _) -> Just $ Failure $ describeFailure f
                    (Right msg, _) -> Just $ Info msg
                }
        s2 =
            s1
                { stProfileCursor =
                    clamp (length $ profilesOf s1) $ stProfileCursor s1
                , stNotificationCursor =
                    clamp (length $ notificationsOf s1) $
                        stNotificationCursor s1
                }
    in  advanceWizard resultJob resultOutcome resultSnapshot s2

{- | Move a guided install forward after one of its jobs finished.
A failure closes it; success advances by the step that ran.
-}
advanceWizard
    :: Job
    -> Either LpacFailure Text
    -> Maybe (Either LpacFailure Snapshot)
    -> State
    -> (State, Maybe Job)
advanceWizard job outcome snapshot s = case stWizard s of
    Nothing -> (s, Nothing)
    Just w -> case job of
        -- Reading the QR is part of the source step; its failure
        -- leaves the wizard where it is.
        DecodeQr _ -> (s, Nothing)
        _ -> case outcome of
            Left _ -> (s{stWizard = Nothing}, Nothing)
            Right _ -> case job of
                Download _ _ -> case newProfileOf w snapshot of
                    [p] ->
                        ( s
                            { stWizard =
                                Just
                                    w
                                        { wzPhase = WzNickname
                                        , wzNew = Just p
                                        , wzNicknameInput = ""
                                        }
                            }
                        , Nothing
                        )
                    _ -> (s{stWizard = Nothing}, Nothing)
                Nickname _ _ ->
                    (s{stConfirm = wzNew w}, Nothing)
                Enable _ ->
                    let seqs = case snapshot of
                            Just (Right snap) ->
                                map notificationSeq $ snapNotifications snap
                            _ -> []
                    in  if null seqs
                            then (s{stWizard = Just w{wzPhase = WzDone}}, Nothing)
                            else
                                ( s{stWizard = Just w}
                                , Just $ SendNotifications seqs
                                )
                SendNotifications _ ->
                    (s{stWizard = Just w{wzPhase = WzDone}}, Nothing)
                _ -> (s, Nothing)

-- | The profiles that appeared with the latest card read.
newProfileOf
    :: Wizard -> Maybe (Either LpacFailure Snapshot) -> [Profile]
newProfileOf w = \case
    Just (Right snap) ->
        filter
            (\p -> profileIccid p `notElem` wzKnownIccids w)
            $ snapProfiles snap
    _ -> []

-- | Put a decoded activation code into the form, masked.
fillFrom :: DownloadTarget -> State -> State
fillFrom target s =
    s
        { stForm =
            (stForm s)
                { formSmdp = targetSmdp target
                , formCode = revealSecret $ targetMatchingId target
                , formFocus = CodeField
                }
        }

focus :: Field -> Form -> Form
focus f form = form{formFocus = f}

focusCode :: Form -> Form
focusCode = focus CodeField

editConfirm :: Wizard -> Wizard
editConfirm w = w{wzConfirmInput = T.dropEnd 1 $ wzConfirmInput w}

snocConfirm :: Wizard -> Char -> Wizard
snocConfirm w c = w{wzConfirmInput = T.snoc (wzConfirmInput w) c}

editNicknameInput :: Wizard -> Wizard
editNicknameInput w =
    w{wzNicknameInput = T.dropEnd 1 $ wzNicknameInput w}

snocNickname :: Wizard -> Char -> Wizard
snocNickname w c = w{wzNicknameInput = T.snoc (wzNicknameInput w) c}

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

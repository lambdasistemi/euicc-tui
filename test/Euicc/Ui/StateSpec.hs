module Euicc.Ui.StateSpec (spec) where

import Data.Text (Text)
import Data.Text qualified as T
import Euicc.ActivationCode
    ( DownloadTarget (..)
    , mkSecret
    , revealSecret
    )
import Euicc.Job
    ( Job (..)
    , JobResult (..)
    , Snapshot (..)
    , jobResult
    )
import Euicc.Lpac.Output
    ( ChipInfo (..)
    , LpacFailure (..)
    , Profile (..)
    , parseNotifications
    , parseProfiles
    )
import Euicc.Ui.State
    ( Browser (..)
    , Field (..)
    , Form (..)
    , State (..)
    , Status (..)
    , Step (..)
    , View (..)
    , Wizard (..)
    , WizardPhase (..)
    , codeDisplay
    , finishJob
    , handleKey
    , selectedProfile
    , smdpDisplay
    , start
    )
import Fixtures (fixtureOk)
import Graphics.Vty (Key (..))
import Test.Hspec
    ( Spec
    , beforeAll
    , describe
    , it
    , shouldBe
    , shouldSatisfy
    )
import Test.QuickCheck
    ( Gen
    , elements
    , forAll
    , listOf
    , property
    )

-- | The card as recorded in the fixtures.
loadedSnapshot :: IO Snapshot
loadedSnapshot = do
    Right ps <- parseProfiles <$> fixtureOk "profile-list"
    Right ns <- parseNotifications <$> fixtureOk "notification-list"
    pure
        Snapshot
            { snapChip = ChipInfo "89049032000001000000000000000123" Nothing
            , snapProfiles = ps
            , snapNotifications = ns
            }

-- | The state after the first refresh completed.
loaded :: IO State
loaded = do
    snap <- loadedSnapshot
    let (s, _) = start
    pure $
        apply (jobResult Refresh (Right "loaded") (Just (Right snap))) s

-- | Apply a finished job to a state.
apply :: JobResult -> State -> State
apply r s = fst (finishJob r s)

-- | A snapshot with one extra disabled profile, as after a download.
snapshotWithNew :: IO Snapshot
snapshotWithNew = do
    snap <- loadedSnapshot
    let newProfile =
            case drop 1 $ snapProfiles snap of
                (p : _) ->
                    p
                        { profileIccid = "8900000000000000099"
                        , profileNickname = Nothing
                        , profileName = Just "Purchased plan"
                        }
                [] -> error "fixture profile list is too short"
    pure snap{snapProfiles = snapProfiles snap <> [newProfile]}

-- | The wizard with a listing loaded, cursor on the first entry.
atListing :: IO State
atListing = do
    s0 <- loaded
    let (s1, _) = pressAll [KChar 'g', KEnter] s0
        result =
            JobResult
                { resultJob = ReadDir "."
                , resultOutcome = Right "Directory read."
                , resultSnapshot = Nothing
                , resultQr = Nothing
                , resultDir =
                    Just
                        ( "/home/op"
                        , [(True, "Downloads"), (False, "cuniq.png")]
                        )
                }
    pure $ apply result s1

-- | Keys that submit a plain activation code in the wizard.
submitKeys :: [Key]
submitKeys =
    [KChar 'g', KChar '\t', KChar '\t']
        <> typeText "LPA:1$smdp.example.com$AB-12"
        <> [KEnter]

-- | Stand-in values for results whose payload is irrelevant.
dummyTarget :: DownloadTarget
dummyTarget = DownloadTarget "x.example" (mkSecret "X") False

-- | The wizard after a QR code was read, waiting for Enter.
atReady :: Bool -> IO State
atReady confirmation = do
    s0 <- loaded
    let (s1, _) = pressAll ([KChar 'g'] <> typeText "plan.png" <> [KEnter]) s0
        target =
            DownloadTarget
                { targetSmdp = "qr-smdp.example.org"
                , targetMatchingId = mkSecret "QR-MATCH-7X"
                , targetConfirmationRequired = confirmation
                }
    pure $
        apply
            JobResult
                { resultJob = DecodeQr "plan.png"
                , resultOutcome = Right "QR code read."
                , resultSnapshot = Nothing
                , resultQr = Just target
                , resultDir = Nothing
                }
            s1

-- | Press one key, expecting the program to continue.
press :: Key -> State -> (State, Maybe Job)
press k s = case handleKey k [] s of
    Continue s' j -> (s', j)
    Halt -> error "unexpected halt"

-- | Press keys in order, collecting the jobs they start.
pressAll :: [Key] -> State -> (State, [Job])
pressAll ks s0 = foldl' go (s0, []) ks
  where
    go (s, js) k = let (s', j) = press k s in (s', js <> maybe [] pure j)

typeText :: Text -> [Key]
typeText = map KChar . T.unpack

genKey :: Gen Key
genKey =
    elements $
        [KEnter, KEsc, KUp, KDown, KBS, KChar '\t']
            <> map KChar "abcdeijkmnpqrsxygzDLPA:01$.-?"

spec :: Spec
spec = do
    describe "start" $ do
        it "begins by reading the card" $
            snd start `shouldBe` Refresh
    describe "profiles view" $ do
        it "asks for confirmation before enabling" $ do
            s0 <- loaded
            let (s1, j1) = pressAll [KDown, KChar 'e'] s0
            j1 `shouldBe` []
            fmap profileIccid (stConfirm s1)
                `shouldBe` Just "8939100000000000001"
        it "enables after y" $ do
            s0 <- loaded
            let (s1, js) = pressAll [KDown, KChar 'e', KChar 'y'] s0
            map jobIccid js `shouldBe` [Just "8939100000000000001"]
            stBusy s1 `shouldSatisfy` (/= Nothing)
            stConfirm s1 `shouldBe` Nothing
        it "does nothing after n" $ do
            s0 <- loaded
            let (s1, js) = pressAll [KDown, KChar 'e', KChar 'n'] s0
            js `shouldBe` []
            stConfirm s1 `shouldBe` Nothing
        it "does not re-enable the enabled profile" $ do
            s0 <- loaded
            let (s1, js) = pressAll [KChar 'e', KChar 'y'] s0
            js `shouldBe` []
            stStatus s1 `shouldSatisfy` \case
                Just (Info _) -> True
                _ -> False
        it "moves the cursor within bounds" $ do
            s0 <- loaded
            let (s1, _) = pressAll (replicate 5 KDown) s0
            fmap profileIccid (selectedProfile s1)
                `shouldBe` Just "8939100000000000001"
            let (s2, _) = pressAll (replicate 5 KUp) s1
            fmap profileIccid (selectedProfile s2)
                `shouldBe` Just "8944476500001234567"
        it "refreshes on r" $ do
            s0 <- loaded
            snd (press (KChar 'r') s0) `shouldBe` Just Refresh
        it "quits on q" $ do
            s0 <- loaded
            handleKey (KChar 'q') [] s0 `shouldBe` Halt
    describe "delete" $ do
        it "refuses the enabled profile" $ do
            s0 <- loaded
            let (s1, js) = pressAll [KChar 'D'] s0
            js `shouldBe` []
            stDelete s1 `shouldBe` Nothing
            stStatus s1 `shouldSatisfy` \case
                Just (Info _) -> True
                _ -> False
        it "asks for the ICCID's last digits on a disabled profile" $ do
            s0 <- loaded
            let (s1, js) = pressAll [KDown, KChar 'D'] s0
            js `shouldBe` []
            fmap (profileIccid . fst) (stDelete s1)
                `shouldBe` Just "8939100000000000001"
        it "deletes when the digits match" $ do
            s0 <- loaded
            let (s1, js) =
                    pressAll ([KDown, KChar 'D'] <> typeText "0001" <> [KEnter]) s0
            js `shouldSatisfy` \case
                [Delete p] -> profileIccid p == "8939100000000000001"
                _ -> False
            stDelete s1 `shouldBe` Nothing
        it "does not delete when the digits differ" $ do
            s0 <- loaded
            let (s1, js) =
                    pressAll ([KDown, KChar 'D'] <> typeText "0002" <> [KEnter]) s0
            js `shouldBe` []
            stDelete s1 `shouldBe` Nothing
        it "does not delete on Enter alone" $ do
            s0 <- loaded
            let (_, js) = pressAll [KDown, KChar 'D', KEnter] s0
            js `shouldBe` []
        it "cancels on Esc" $ do
            s0 <- loaded
            let (s1, js) =
                    pressAll ([KDown, KChar 'D'] <> typeText "0001" <> [KEsc]) s0
            js `shouldBe` []
            stDelete s1 `shouldBe` Nothing
        it "treats y as a digit guess, not a confirmation" $ do
            s0 <- loaded
            let (_, js) = pressAll [KDown, KChar 'D', KChar 'y', KEnter] s0
            js `shouldBe` []
    describe "help" $ do
        it "opens on ? over the profiles" $ do
            s0 <- loaded
            let (s1, js) = pressAll [KChar '?'] s0
            js `shouldBe` []
            stHelp s1 `shouldBe` True
        it "closes on the next key, which does nothing else" $ do
            s0 <- loaded
            case handleKey (KChar 'q') [] (fst $ pressAll [KChar '?'] s0) of
                Continue s2 j -> do
                    j `shouldBe` Nothing
                    stHelp s2 `shouldBe` False
                Halt -> error "q closed the program under the help"
        it "opens on ? even in a text field, leaving it untouched" $ do
            s0 <- loaded
            let (s1, _) = pressAll [KChar 'd', KChar '?'] s0
            stHelp s1 `shouldBe` True
            formSmdp (stForm s1) `shouldBe` ""
    describe "busy" $ do
        it "starts no job while one is running" $ do
            s0 <- loaded
            let (s1, _) = press (KChar 'r') s0
            snd (pressAll [KChar 'r', KDown, KChar 'e', KChar 'y'] s1)
                `shouldBe` []
        it "is cleared by the job's result" $ do
            s0 <- loaded
            snap <- loadedSnapshot
            let (s1, _) = press (KChar 'r') s0
                s2 = apply (jobResult Refresh (Right "ok") (Just (Right snap))) s1
            stBusy s2 `shouldBe` Nothing
    describe "results" $ do
        it "shows a failed action as a failure message" $ do
            s0 <- loaded
            snap <- loadedSnapshot
            let s1 =
                    apply
                        ( jobResult
                            Refresh
                            (Left $ LpacError "es9p_handle_notification" "")
                            (Just (Right snap))
                        )
                        s0
            stStatus s1 `shouldSatisfy` \case
                Just (Failure _) -> True
                _ -> False
        it "stays on the profiles view while the card is unreadable" $ do
            let (s, _) = start
                s1 = apply (jobResult Refresh (Left NoReader) (Just (Left NoReader))) s
            stView (fst $ pressAll [KChar 'd'] s1) `shouldBe` ProfilesView
            stView (fst $ pressAll [KChar 'n'] s1) `shouldBe` ProfilesView
        it "does not repeat the card failure on the status line" $ do
            let (s, _) = start
                s1 = apply (jobResult Refresh (Left NoReader) (Just (Left NoReader))) s
            stStatus s1 `shouldBe` Nothing
        it "keeps a missing reader as the card state" $ do
            let (s, _) = start
                s1 = apply (jobResult Refresh (Left NoReader) (Just (Left NoReader))) s
            stCard s1 `shouldBe` Just (Left NoReader)
        it "clamps the cursor when profiles disappear" $ do
            s0 <- loaded
            snap <- loadedSnapshot
            let (s1, _) = press KDown s0
                s2 =
                    apply
                        ( jobResult
                            Refresh
                            (Right "ok")
                            (Just (Right snap{snapProfiles = take 1 $ snapProfiles snap}))
                        )
                        s1
            fmap profileIccid (selectedProfile s2)
                `shouldBe` Just "8944476500001234567"
    describe "notifications view" $ do
        it "sends the selected notification" $ do
            s0 <- loaded
            snd (pressAll [KChar 'n', KDown, KChar 's'] s0)
                `shouldBe` [SendNotifications [8]]
        it "sends all notifications" $ do
            s0 <- loaded
            snd (pressAll [KChar 'n', KChar 'a'] s0)
                `shouldBe` [SendNotifications [7, 8]]
        it "sends nothing when there is nothing pending" $ do
            s0 <- loaded
            snap <- loadedSnapshot
            let s1 =
                    apply
                        ( jobResult
                            Refresh
                            (Right "ok")
                            (Just (Right snap{snapNotifications = []}))
                        )
                        s0
            snd (pressAll [KChar 'n', KChar 'a', KChar 's'] s1)
                `shouldBe` []
        it "goes back with Esc" $ do
            s0 <- loaded
            stView (fst $ pressAll [KChar 'n', KEsc] s0)
                `shouldBe` ProfilesView
    describe "guided install" $ do
        it "reads a QR image from the first field on Enter" $ do
            s0 <- loaded
            let keys =
                    [KChar 'g']
                        <> typeText "plan.png"
                        <> [KEnter]
                (s1, js) = pressAll keys s0
            js `shouldBe` [DecodeQr "plan.png"]
            stStatus s1 `shouldSatisfy` \case
                Just (Info _) -> True
                _ -> False
        it "fills the form from a decoded QR code, masked" $ do
            s0 <- loaded
            let (s1, _) = pressAll [KChar 'g', KEnter] s0
                target =
                    DownloadTarget
                        { targetSmdp = "qr-smdp.example.org"
                        , targetMatchingId = mkSecret "QR-MATCH-7X"
                        , targetConfirmationRequired = False
                        }
                s2 =
                    apply
                        JobResult
                            { resultJob = DecodeQr "plan.png"
                            , resultOutcome = Right "QR code read."
                            , resultSnapshot = Nothing
                            , resultQr = Just target
                            , resultDir = Nothing
                            }
                        s1
            formSmdp (stForm s2) `shouldBe` "qr-smdp.example.org"
            formCode (stForm s2) `shouldBe` "QR-MATCH-7X"
            codeDisplay (stForm s2) `shouldBe` "***********"
            formFocus (stForm s2) `shouldBe` CodeField
            stView s2 `shouldBe` WizardView
        it "shows a QR failure without leaving the wizard" $ do
            s0 <- loaded
            let (s1, _) = pressAll [KChar 'g', KEnter] s0
                s2 =
                    apply
                        ( jobResult
                            (DecodeQr "plan.png")
                            (Left $ QrDecode "no QR code found in this image.")
                            Nothing
                        )
                        s1
            stView s2 `shouldBe` WizardView
            fmap wzPhase (stWizard s2) `shouldBe` Just WzSource
            stStatus s2 `shouldSatisfy` \case
                Just (Failure _) -> True
                _ -> False
        it "opens on g, remembering the card's ICCIDs" $ do
            s0 <- loaded
            let (s1, js) = pressAll [KChar 'g'] s0
            js `shouldBe` []
            stView s1 `shouldBe` WizardView
            fmap wzKnownIccids (stWizard s1)
                `shouldBe` Just
                    ["8944476500001234567", "8939100000000000001"]
            fmap wzPhase (stWizard s1) `shouldBe` Just WzSource
        it "closes on Esc, back to the profiles" $ do
            s0 <- loaded
            let (s1, _) = pressAll [KChar 'g', KEsc] s0
            stView s1 `shouldBe` ProfilesView
            stWizard s1 `shouldBe` Nothing
            formCode (stForm s1) `shouldBe` ""
        it "refuses to open before the card is read" $ do
            let (s, _) = start
                (s1, js) = pressAll [KChar 'g'] s
            js `shouldBe` []
            stView s1 `shouldBe` ProfilesView
        it "asks for the confirmation code the activation code demands" $ do
            s0 <- loaded
            let keys =
                    [KChar 'g', KChar '\t', KChar '\t']
                        <> typeText "LPA:1$confirm.example.org$MID-9$1.2.3$1"
                        <> [KEnter]
                (s1, js) = pressAll keys s0
            js `shouldBe` []
            fmap wzPhase (stWizard s1) `shouldBe` Just WzConfirm
        it "downloads with the typed confirmation code" $ do
            s0 <- loaded
            let keys =
                    [KChar 'g', KChar '\t', KChar '\t']
                        <> typeText "LPA:1$confirm.example.org$MID-9$1.2.3$1"
                        <> [KEnter]
                        <> typeText "C-7"
                        <> [KEnter]
                (s1, js) = pressAll keys s0
            js
                `shouldBe` [ Download
                                ( DownloadTarget
                                    "confirm.example.org"
                                    (mkSecret "MID-9")
                                    True
                                )
                                (Just $ mkSecret "C-7")
                           ]
            fmap wzConfirmInput (stWizard s1) `shouldBe` Just ""
            formCode (stForm s1) `shouldBe` ""
            show s1 `shouldSatisfy` (not . T.isInfixOf "C-7" . T.pack)
        it "waits for Enter after reading the QR code" $ do
            s1 <- atReady False
            fmap wzPhase (stWizard s1) `shouldBe` Just WzReady
            stView s1 `shouldBe` WizardView
        it "installs the read plan on Enter" $ do
            s1 <- atReady False
            let (_, js) = pressAll [KEnter] s1
            js
                `shouldBe` [ Download
                                ( DownloadTarget
                                    "qr-smdp.example.org"
                                    (mkSecret "QR-MATCH-7X")
                                    False
                                )
                                Nothing
                           ]
        it "cancels the read plan on Esc" $ do
            s1 <- atReady False
            let (s2, js) = pressAll [KEsc] s1
            js `shouldBe` []
            stView s2 `shouldBe` ProfilesView
            stWizard s2 `shouldBe` Nothing
            formCode (stForm s2) `shouldBe` ""
        it "asks the confirmation code of a read plan on Enter" $ do
            s1 <- atReady True
            let (s2, js) = pressAll [KEnter] s1
            js `shouldBe` []
            fmap wzPhase (stWizard s2) `shouldBe` Just WzConfirm
        it "returns to the list on the new plan after a download" $ do
            s0 <- loaded
            snap' <- snapshotWithNew
            let (s2, _) = pressAll submitKeys s0
                s3 =
                    apply
                        ( jobResult
                            (Download dummyTarget Nothing)
                            (Right "Profile downloaded.")
                            (Just (Right snap'))
                        )
                        s2
            stView s3 `shouldBe` ProfilesView
            stWizard s3 `shouldBe` Nothing
            fmap profileIccid (selectedProfile s3)
                `shouldBe` Just "8900000000000000099"
            stStatus s3 `shouldSatisfy` \case
                Just (Info t) -> "Installed" `T.isPrefixOf` t
                _ -> False
        it "returns to the list when the download reveals no new profile" $ do
            s0 <- loaded
            snap <- loadedSnapshot
            let (s2, _) = pressAll submitKeys s0
                s3 =
                    apply
                        ( jobResult
                            (Download dummyTarget Nothing)
                            (Right "Profile downloaded.")
                            (Just (Right snap))
                        )
                        s2
            stView s3 `shouldBe` ProfilesView
            stWizard s3 `shouldBe` Nothing
        it "a failed download returns to the list with the failure" $ do
            s0 <- loaded
            let (s2, _) = pressAll submitKeys s0
                s3 =
                    apply
                        ( jobResult
                            (Download dummyTarget Nothing)
                            (Left $ LpacError "es9p_plus" "refused")
                            Nothing
                        )
                        s2
            stView s3 `shouldBe` ProfilesView
            stWizard s3 `shouldBe` Nothing
            stStatus s3 `shouldSatisfy` \case
                Just (Failure _) -> True
                _ -> False
    describe "nickname" $ do
        it "edits the selected profile's nickname on m" $ do
            s0 <- loaded
            let (s1, _) = pressAll [KDown, KChar 'm'] s0
            stNicknameEdit s1
                `shouldSatisfy` \case
                    Just (p, "") -> profileIccid p == "8939100000000000001"
                    _ -> False
        it "starts from the existing nickname" $ do
            s0 <- loaded
            let (s1, _) = pressAll [KChar 'm'] s0
            stNicknameEdit s1
                `shouldSatisfy` \case
                    Just (p, "travel") -> profileIccid p == "8944476500001234567"
                    _ -> False
        it "sets the nickname on Enter" $ do
            s0 <- loaded
            let (s1, js) =
                    pressAll
                        ([KDown, KChar 'm'] <> typeText "holiday" <> [KEnter])
                        s0
            js `shouldSatisfy` \case
                [Nickname p "holiday"] ->
                    profileIccid p == "8939100000000000001"
                _ -> False
            stNicknameEdit s1 `shouldBe` Nothing
        it "types and backspaces" $ do
            s0 <- loaded
            let (s1, _) = pressAll [KDown, KChar 'm'] s0
                (s2, _) = pressAll (typeText "ab" <> [KBS]) s1
            stNicknameEdit s2
                `shouldBe` fmap (,"a") (selectedProfile s2)
        it "cancels on Esc" $ do
            s0 <- loaded
            let (s1, js) = pressAll [KDown, KChar 'm', KEsc] s0
            js `shouldBe` []
            stNicknameEdit s1 `shouldBe` Nothing
    describe "QR picker" $ do
        it "opens on Enter over an empty QR field" $ do
            s0 <- loaded
            let (s1, js) = pressAll [KChar 'g', KEnter] s0
            js `shouldBe` [ReadDir "."]
            stBrowser s1 `shouldSatisfy` (/= Nothing)
        it "fills from a finished listing" $ do
            s0 <- loaded
            let (s1, _) = pressAll [KChar 'g', KEnter] s0
                s2 =
                    apply
                        JobResult
                            { resultJob = ReadDir "."
                            , resultOutcome = Right "Directory read."
                            , resultSnapshot = Nothing
                            , resultQr = Nothing
                            , resultDir =
                                Just
                                    ( "/home/op"
                                    , [(True, "Downloads"), (False, "cuniq.png")]
                                    )
                            }
                        s1
            stBrowser s2
                `shouldBe` Just
                    (Browser "/home/op" [(True, "Downloads"), (False, "cuniq.png")] 0)
        it "moves the cursor within the listing" $ do
            s1 <- atListing
            let (s2, _) = pressAll [KDown, KDown, KDown] s1
            brCursor <$> stBrowser s2 `shouldBe` Just 1
            let (s3, _) = pressAll [KUp] s2
            brCursor <$> stBrowser s3 `shouldBe` Just 0
        it "picks a file: fills the path and starts the read" $ do
            s1 <- atListing
            let (s2, js) = pressAll [KDown, KEnter] s1
            js `shouldBe` [DecodeQr "/home/op/cuniq.png"]
            stBrowser s2 `shouldBe` Nothing
            formQr (stForm s2) `shouldBe` "/home/op/cuniq.png"
        it "descends into a directory" $ do
            s1 <- atListing
            let (_, js) = pressAll [KEnter] s1
            js `shouldBe` [ReadDir "/home/op/Downloads"]
        it "goes to the parent on backspace" $ do
            s1 <- atListing
            let (_, js) = pressAll [KBS] s1
            js `shouldBe` [ReadDir "/home"]
        it "cancels on Esc" $ do
            s1 <- atListing
            let (s2, js) = pressAll [KEsc] s1
            js `shouldBe` []
            stBrowser s2 `shouldBe` Nothing
            stView s2 `shouldBe` WizardView
    describe "download form" $ do
        it "downloads from typed address and code" $ do
            s0 <- loaded
            let keys =
                    [KChar 'd']
                        <> typeText "smdp.example.com"
                        <> [KChar '\t']
                        <> typeText "AB-12"
                        <> [KEnter]
                (s1, js) = pressAll keys s0
            map jobTarget js `shouldBe` [Just ("smdp.example.com", "AB-12")]
            formCode (stForm s1) `shouldBe` ""
        it "accepts a pasted LPA string in the code field" $ do
            s0 <- loaded
            let keys =
                    [KChar 'd', KChar '\t']
                        <> typeText "LPA:1$smdp.example.com$AB-12"
                        <> [KEnter]
            map jobTarget (snd $ pressAll keys s0)
                `shouldBe` [Just ("smdp.example.com", "AB-12")]
        it "treats q as text, not quit" $ do
            s0 <- loaded
            let (s1, _) = pressAll [KChar 'd', KChar 'q'] s0
            formSmdp (stForm s1) `shouldBe` "q"
        it "edits with backspace" $ do
            s0 <- loaded
            let (s1, _) = pressAll ([KChar 'd'] <> typeText "abc" <> [KBS]) s0
            formSmdp (stForm s1) `shouldBe` "ab"
        it "reports an invalid code without starting a job" $ do
            s0 <- loaded
            let (s1, js) =
                    pressAll
                        ([KChar 'd', KChar '\t'] <> typeText "LPA:9$x" <> [KEnter])
                        s0
            js `shouldBe` []
            stStatus s1 `shouldSatisfy` \case
                Just (Failure _) -> True
                _ -> False
        it "masks the code on screen" $ do
            s0 <- loaded
            let (s1, _) =
                    pressAll ([KChar 'd', KChar '\t'] <> typeText "AB-12") s0
            codeDisplay (stForm s1) `shouldBe` "*****"
            formFocus (stForm s1) `shouldBe` CodeField
        it "masks the code inside an LPA string in the address field" $ do
            s0 <- loaded
            let (s1, _) =
                    pressAll
                        ([KChar 'd'] <> typeText "LPA:1$smdp.example.com$AB-12")
                        s0
            smdpDisplay (stForm s1)
                `shouldBe` "LPA:1$smdp.example.com$*****"
        it "shows a plain address as typed" $ do
            s0 <- loaded
            let (s1, _) = pressAll ([KChar 'd'] <> typeText "a.com") s0
            smdpDisplay (stForm s1) `shouldBe` "a.com"
        it "never shows the code in the state's text form" $ do
            s0 <- loaded
            let (s1, _) =
                    pressAll ([KChar 'd', KChar '\t'] <> typeText "ZQ-SECRET") s0
            show s1 `shouldSatisfy` (not . T.isInfixOf "ZQ-SECRET" . T.pack)
        it "never shows a code pasted in the address field" $ do
            s0 <- loaded
            let (s1, _) =
                    pressAll
                        ([KChar 'd'] <> typeText "LPA:1$a.com$ZQ-SECRET")
                        s0
            show s1 `shouldSatisfy` (not . T.isInfixOf "ZQ-SECRET" . T.pack)
        it "clears the code when cancelled" $ do
            s0 <- loaded
            let (s1, _) =
                    pressAll ([KChar 'd', KChar '\t'] <> typeText "AB" <> [KEsc]) s0
            stView s1 `shouldBe` ProfilesView
            formCode (stForm s1) `shouldBe` ""
    beforeAll ((,) <$> loaded <*> loadedSnapshot) $
        describe "safety" $
            it "enables only on y; downloads, decodes and names only on Enter" $
                \(s0, snap) ->
                    property $ forAll (listOf genKey) $ \ks ->
                        let step (s, ok) k = case handleKey k [] s of
                                Halt -> (s, ok)
                                Continue s' j ->
                                    let done =
                                            maybe
                                                s'
                                                ( const $
                                                    apply
                                                        (jobResult Refresh (Right "ok") (Just (Right snap)))
                                                        s'
                                                )
                                                j
                                    in  (done, ok && all (allowed k) j)
                            allowed k = \case
                                Enable _ -> k == KChar 'y'
                                Delete _ -> k == KEnter
                                Download _ _ -> k == KEnter
                                DecodeQr _ -> k == KEnter
                                Nickname _ _ -> k == KEnter
                                ReadDir _ -> k == KEnter || k == KBS
                                _ -> True
                        in  snd $ foldl' step (s0, True) ks
  where
    jobIccid = \case
        Enable p -> Just $ profileIccid p
        _ -> Nothing
    jobTarget = \case
        Download DownloadTarget{..} _ ->
            Just (targetSmdp, revealSecret targetMatchingId)
        _ -> Nothing

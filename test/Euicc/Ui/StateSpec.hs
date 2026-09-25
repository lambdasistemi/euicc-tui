module Euicc.Ui.StateSpec (spec) where

import Data.Text (Text)
import Data.Text qualified as T
import Euicc.ActivationCode (DownloadTarget (..), revealSecret)
import Euicc.Job (Job (..), Snapshot (..), jobResult)
import Euicc.Lpac.Output
    ( ChipInfo (..)
    , LpacFailure (..)
    , Profile (..)
    , parseNotifications
    , parseProfiles
    )
import Euicc.Ui.State
    ( Field (..)
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
    pure $ finishJob (jobResult Refresh (Right "loaded") (Just (Right snap))) s

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
            <> map KChar "abcdeijknpqrsxyzDLPA:1$.-"

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
                s2 = finishJob (jobResult Refresh (Right "ok") (Just (Right snap))) s1
            stBusy s2 `shouldBe` Nothing
    describe "results" $ do
        it "shows a failed action as a failure message" $ do
            s0 <- loaded
            snap <- loadedSnapshot
            let s1 =
                    finishJob
                        ( jobResult Refresh
                            (Left $ LpacError "es9p_handle_notification" "")
                            (Just (Right snap))
                        )
                        s0
            stStatus s1 `shouldSatisfy` \case
                Just (Failure _) -> True
                _ -> False
        it "stays on the profiles view while the card is unreadable" $ do
            let (s, _) = start
                s1 = finishJob (jobResult Refresh (Left NoReader) (Just (Left NoReader))) s
            stView (fst $ pressAll [KChar 'd'] s1) `shouldBe` ProfilesView
            stView (fst $ pressAll [KChar 'n'] s1) `shouldBe` ProfilesView
        it "does not repeat the card failure on the status line" $ do
            let (s, _) = start
                s1 = finishJob (jobResult Refresh (Left NoReader) (Just (Left NoReader))) s
            stStatus s1 `shouldBe` Nothing
        it "keeps a missing reader as the card state" $ do
            let (s, _) = start
                s1 = finishJob (jobResult Refresh (Left NoReader) (Just (Left NoReader))) s
            stCard s1 `shouldBe` Just (Left NoReader)
        it "clamps the cursor when profiles disappear" $ do
            s0 <- loaded
            snap <- loadedSnapshot
            let (s1, _) = press KDown s0
                s2 =
                    finishJob
                        ( jobResult Refresh
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
                    finishJob
                        (jobResult Refresh (Right "ok") (Just (Right snap{snapNotifications = []})))
                        s0
            snd (pressAll [KChar 'n', KChar 'a', KChar 's'] s1)
                `shouldBe` []
        it "goes back with Esc" $ do
            s0 <- loaded
            stView (fst $ pressAll [KChar 'n', KEsc] s0)
                `shouldBe` ProfilesView
    describe "guided install" $ do
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
            it "enables only on y and downloads only on Enter" $
                \(s0, snap) ->
                    property $ forAll (listOf genKey) $ \ks ->
                        let step (s, ok) k = case handleKey k [] s of
                                Halt -> (s, ok)
                                Continue s' j ->
                                    let done =
                                            maybe
                                                s'
                                                ( const $
                                                    finishJob
                                                        (jobResult Refresh (Right "ok") (Just (Right snap)))
                                                        s'
                                                )
                                                j
                                    in  (done, ok && all (allowed k) j)
                            allowed k = \case
                                Enable _ -> k == KChar 'y'
                                Download _ _ -> k == KEnter
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

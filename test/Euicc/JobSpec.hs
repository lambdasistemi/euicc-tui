module Euicc.JobSpec (spec) where

import Data.IORef (modifyIORef, newIORef, readIORef)
import Data.List (sort)
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Euicc.ActivationCode (DownloadTarget (..), mkSecret)
import Euicc.Job
    ( Job (..)
    , JobResult (..)
    , LpacRunner (..)
    , Snapshot (..)
    , runJob
    )
import Euicc.Lpac.Command (Command (..))
import Euicc.Lpac.Output
    ( ChipInfo (..)
    , LpacFailure (..)
    , Profile (..)
    , RawOutput
    , describeFailure
    , parseProfiles
    , profileLabel
    )
import Fixtures (fixture, fixtureOk)
import System.Exit (ExitCode (..))
import Test.Hspec
    ( Spec
    , describe
    , it
    , shouldBe
    , shouldContain
    , shouldSatisfy
    )

-- | A card that answers reads from fixtures and actions as given.
fakeCard
    :: (Command -> Maybe (IO RawOutput)) -> Command -> IO RawOutput
fakeCard action = \case
    ReadChipInfo -> fixtureOk "chip-info"
    ListProfiles -> fixtureOk "profile-list"
    ListNotifications -> fixtureOk "notification-list"
    c -> case action c of
        Just out -> out
        Nothing -> error $ "unexpected command " <> show c

-- | Run a job against a fake card, recording the commands issued.
runRecorded
    :: (Command -> Maybe (IO RawOutput))
    -> Job
    -> IO (JobResult, [Command])
runRecorded action job = do
    ref <- newIORef []
    let runner = LpacRunner $ \c -> do
            modifyIORef ref (c :)
            fakeCard action c
    r <- runJob runner job
    cmds <- reverse <$> readIORef ref
    pure (r, cmds)

disabledProfile :: IO Profile
disabledProfile = do
    Right [_, p] <- parseProfiles <$> fixtureOk "profile-list"
    pure p

-- | The card state of a result, for assertions.
snapOf :: JobResult -> Either LpacFailure Snapshot
snapOf = fromMaybe (Left (UnexpectedOutput "no card read")) . resultSnapshot

reads' :: [Command]
reads' = [ReadChipInfo, ListProfiles, ListNotifications]

entryNames :: [(Bool, T.Text)] -> [T.Text]
entryNames = map snd

noDotEntries :: [(Bool, T.Text)] -> Bool
noDotEntries = all (\(_, name) -> name /= "." && name /= "..")

spec :: Spec
spec = do
    describe "Refresh" $ do
        it "reads chip info, profiles and notifications" $ do
            (r, cmds) <- runRecorded (const Nothing) Refresh
            cmds `shouldBe` reads'
            fmap snapChip (snapOf r)
                `shouldBe` Right
                    (ChipInfo "89049032000001000000000000000123" (Just 291740))
            fmap (length . snapProfiles) (snapOf r)
                `shouldBe` Right 2
            fmap (length . snapNotifications) (snapOf r)
                `shouldBe` Right 2
        it "reports a missing reader without crashing" $ do
            let runner = LpacRunner $ \_ ->
                    fixture (ExitFailure 255) "no-reader"
            r <- runJob runner Refresh
            snapOf r `shouldBe` Left NoReader
    describe "Enable" $ do
        it "enables by ICCID, then reloads" $ do
            p <- disabledProfile
            (r, cmds) <-
                runRecorded
                    ( \case
                        EnableProfile _ -> Just $ fixtureOk "enable-ok"
                        _ -> Nothing
                    )
                    (Enable p)
            cmds `shouldBe` EnableProfile (profileIccid p) : reads'
            resultOutcome r `shouldSatisfy` either (const False) (const True)
        it "reports lpac's refusal and still reloads" $ do
            p <- disabledProfile
            (r, cmds) <-
                runRecorded
                    ( \case
                        EnableProfile _ ->
                            Just $ fixture (ExitFailure 255) "enable-not-found"
                        _ -> Nothing
                    )
                    (Enable p)
            drop 1 cmds `shouldBe` reads'
            resultOutcome r
                `shouldBe` Left
                    ( LpacError
                        "es10c_enable_profile"
                        "iccid or aid not found"
                    )
    describe "SendNotifications" $ do
        it "sends exactly the requested sequence numbers" $ do
            (_, cmds) <-
                runRecorded
                    ( \case
                        ProcessNotifications _ ->
                            Just $ fixtureOk "notification-process-ok"
                        _ -> Nothing
                    )
                    (SendNotifications [7])
            take 1 cmds `shouldBe` [ProcessNotifications [7]]
        it "reports a network failure as an outcome, not a crash" $ do
            (r, _) <-
                runRecorded
                    ( \case
                        ProcessNotifications _ ->
                            Just $
                                fixture
                                    (ExitFailure 255)
                                    "notification-process-offline"
                        _ -> Nothing
                    )
                    (SendNotifications [7, 8])
            resultOutcome r
                `shouldBe` Left (LpacError "es9p_handle_notification" "")
            fmap (length . snapProfiles) (snapOf r)
                `shouldBe` Right 2
    describe "Nickname" $ do
        it "nicknames by ICCID, then reloads" $ do
            p <- disabledProfile
            (r, cmds) <-
                runRecorded
                    ( \case
                        NicknameProfile _ _ ->
                            Just $ fixtureOk "profile-nickname-ok"
                        _ -> Nothing
                    )
                    (Nickname p "holiday")
            cmds
                `shouldBe` NicknameProfile (profileIccid p) "holiday" : reads'
            resultOutcome r `shouldBe` Right ("Named " <> profileLabel p <> ".")
    describe "Download" $ do
        let target =
                DownloadTarget
                    { targetSmdp = "smdp.example.com"
                    , targetMatchingId = mkSecret "SECRET-MATCHING-ID"
                    , targetConfirmationRequired = False
                    }
        it "downloads from the given target" $ do
            (r, cmds) <-
                runRecorded
                    ( \case
                        DownloadProfile _ _ -> Just $ fixtureOk "download-ok"
                        _ -> Nothing
                    )
                    (Download target Nothing)
            cmds `shouldBe` DownloadProfile target Nothing : reads'
            resultOutcome r `shouldSatisfy` either (const False) (const True)
        it "passes the confirmation code to lpac" $ do
            (r, cmds) <-
                runRecorded
                    ( \case
                        DownloadProfile _ _ -> Just $ fixtureOk "download-ok"
                        _ -> Nothing
                    )
                    (Download target $ Just $ mkSecret "C-9")
            take 1 cmds
                `shouldBe` [DownloadProfile target (Just $ mkSecret "C-9")]
            resultOutcome r `shouldSatisfy` either (const False) (const True)
        it "never echoes the matching ID in a failure" $ do
            (r, _) <-
                runRecorded
                    ( \case
                        DownloadProfile _ _ ->
                            Just $
                                fixture (ExitFailure 255) "download-bad-code"
                        _ -> Nothing
                    )
                    (Download target Nothing)
            let shown = either describeFailure id $ resultOutcome r
            shown `shouldSatisfy` (not . T.isInfixOf "SECRET-MATCHING-ID")
            shown `shouldSatisfy` T.isInfixOf "refused"
    describe "ReadDir" $ do
        it "lists a directory without touching the card" $ do
            (r, cmds) <-
                runRecorded (const Nothing) (ReadDir "test/fixtures")
            cmds `shouldBe` []
            resultSnapshot r `shouldBe` Nothing
            case resultDir r of
                Nothing -> error "no directory listing in the result"
                Just (cwd, entries) -> do
                    cwd `shouldBe` "test/fixtures"
                    entryNames entries
                        `shouldContain` ["qr-lpa-ok.png", "qr-none.png"]
                    entryNames entries `shouldSatisfy` (\names -> names == sort names)
                    entries `shouldSatisfy` noDotEntries
        it "reports a missing directory" $ do
            (r, cmds) <-
                runRecorded (const Nothing) (ReadDir "no-such-dir")
            cmds `shouldBe` []
            resultDir r `shouldBe` Nothing
            resultOutcome r `shouldSatisfy` either (const True) (const False)
    describe "DecodeQr" $ do
        it "reads an image without touching the card" $ do
            (r, cmds) <-
                runRecorded
                    (const Nothing)
                    (DecodeQr "test/fixtures/qr-lpa-ok.png")
            cmds `shouldBe` []
            resultOutcome r `shouldBe` Right "QR code read."
            resultSnapshot r `shouldBe` Nothing
            fmap targetSmdp (resultQr r) `shouldBe` Just "qr-smdp.example.org"
        it "reports an image without an activation code" $ do
            (r, cmds) <-
                runRecorded
                    (const Nothing)
                    (DecodeQr "test/fixtures/qr-not-lpa.png")
            cmds `shouldBe` []
            resultSnapshot r `shouldBe` Nothing
            resultQr r `shouldBe` Nothing
            resultOutcome r `shouldSatisfy` \case
                Left (QrDecode _) -> True
                _ -> False

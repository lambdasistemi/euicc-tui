module Euicc.JobSpec (spec) where

import Data.IORef (modifyIORef, newIORef, readIORef)
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
    )
import Fixtures (fixture, fixtureOk)
import System.Exit (ExitCode (..))
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

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

reads' :: [Command]
reads' = [ReadChipInfo, ListProfiles, ListNotifications]

spec :: Spec
spec = do
    describe "Refresh" $ do
        it "reads chip info, profiles and notifications" $ do
            (r, cmds) <- runRecorded (const Nothing) Refresh
            cmds `shouldBe` reads'
            fmap snapChip (resultSnapshot r)
                `shouldBe` Right
                    (ChipInfo "89049032000001000000000000000123" (Just 291740))
            fmap (length . snapProfiles) (resultSnapshot r)
                `shouldBe` Right 2
            fmap (length . snapNotifications) (resultSnapshot r)
                `shouldBe` Right 2
        it "reports a missing reader without crashing" $ do
            let runner = LpacRunner $ \_ ->
                    fixture (ExitFailure 255) "no-reader"
            r <- runJob runner Refresh
            resultSnapshot r `shouldBe` Left NoReader
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
            fmap (length . snapProfiles) (resultSnapshot r)
                `shouldBe` Right 2
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

module Euicc.Lpac.OutputSpec (spec) where

import Data.Text qualified as T
import Euicc.Lpac.Output
    ( ChipInfo (..)
    , LpacFailure (..)
    , Notification (..)
    , Profile (..)
    , ProfileState (..)
    , RawOutput (..)
    , describeFailure
    , parseChipInfo
    , parseDone
    , parseNotifications
    , parseProfiles
    , profileLabel
    )
import Fixtures (fixture, fixtureOk)
import System.Exit (ExitCode (..))
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

failed :: ExitCode
failed = ExitFailure 255

spec :: Spec
spec = do
    describe "parseChipInfo" $ do
        it "reads the EID and free non-volatile memory" $ do
            out <- fixtureOk "chip-info"
            parseChipInfo out
                `shouldBe` Right
                    ChipInfo
                        { chipEid = "89049032000001000000000000000123"
                        , chipFreeMemory = Just 291740
                        }
    describe "parseProfiles" $ do
        it "reads every profile with its state" $ do
            out <- fixtureOk "profile-list"
            parseProfiles out
                `shouldBe` Right
                    [ Profile
                        { profileIccid = "8944476500001234567"
                        , profileAid =
                            "a0000005591010ffffffff8900001000"
                        , profileState = Enabled
                        , profileNickname = Just "travel"
                        , profileProvider = Just "Example Mobile"
                        , profileName = Just "Example Data 5GB"
                        }
                    , Profile
                        { profileIccid = "8939100000000000001"
                        , profileAid =
                            "a0000005591010ffffffff8900001100"
                        , profileState = Disabled
                        , profileNickname = Nothing
                        , profileProvider = Just "Other Telco"
                        , profileName = Just "Home plan"
                        }
                    ]
        it "reads an empty card" $ do
            out <- fixtureOk "profile-list-empty"
            parseProfiles out `shouldBe` Right []
    describe "profileLabel" $ do
        it "prefers the nickname, then the name" $ do
            ps <- parseProfiles <$> fixtureOk "profile-list"
            fmap (map profileLabel) ps
                `shouldBe` Right ["travel", "Home plan"]
        it "falls back to the ICCID" $ do
            ps <- parseProfiles <$> fixtureOk "profile-list"
            fmap
                ( map $ \p ->
                    profileLabel
                        p{profileNickname = Nothing, profileName = Nothing}
                )
                ps
                `shouldBe` Right
                    ["8944476500001234567", "8939100000000000001"]
    describe "parseNotifications" $ do
        it "reads sequence numbers, operations and addresses" $ do
            out <- fixtureOk "notification-list"
            parseNotifications out
                `shouldBe` Right
                    [ Notification
                        7
                        "disable"
                        (Just "rsp.example-mobile.com")
                        (Just "8944476500001234567")
                    , Notification
                        8
                        "enable"
                        (Just "smdp.other-telco.net")
                        (Just "8939100000000000001")
                    ]
        it "reads an empty list" $ do
            out <- fixtureOk "notification-list-empty"
            parseNotifications out `shouldBe` Right []
    describe "parseDone" $ do
        it "accepts a success with null data" $ do
            out <- fixtureOk "enable-ok"
            parseDone out `shouldBe` Right ()
        it "accepts a success after progress lines" $ do
            out <- fixtureOk "notification-process-ok"
            parseDone out `shouldBe` Right ()
        it "reports the failing function and its reason" $ do
            out <- fixture failed "enable-not-found"
            parseDone out
                `shouldBe` Left
                    ( LpacError
                        "es10c_enable_profile"
                        "iccid or aid not found"
                    )
        it "reports a failure after progress lines" $ do
            out <- fixture failed "notification-process-offline"
            parseDone out
                `shouldBe` Left (LpacError "es9p_handle_notification" "")
    describe "failure classification" $ do
        let classified name expected = it name $ do
                out <- fixture failed name
                parseProfiles out `shouldBe` Left expected
        classified "no-reader" NoReader
        classified "no-card" NoCard
        classified "polkit-denied" PolkitDenied
        classified "usb-access" UsbAccessDenied
        classified "wrong-backend" WrongApduBackend
        it "reports unparseable output instead of crashing" $ do
            out <- fixture failed "no-reader"
            parseDone out{rawStderr = "segfault"}
                `shouldSatisfy` \case
                    Left (UnexpectedOutput msg) ->
                        "segfault" `T.isInfixOf` msg
                    _ -> False
        it "rejects a success whose data has the wrong shape" $ do
            out <- fixtureOk "profile-list"
            parseChipInfo out `shouldSatisfy` \case
                Left (UnexpectedOutput _) -> True
                _ -> False
    describe "describeFailure" $ do
        it "tells the operator to re-plug on USB access errors" $
            describeFailure UsbAccessDenied
                `shouldSatisfy` T.isInfixOf "re-plug"
        it "explains polkit denial over SSH" $
            describeFailure PolkitDenied
                `shouldSatisfy` T.isInfixOf "sudo"
        it "names the missing reader" $
            describeFailure NoReader
                `shouldSatisfy` T.isInfixOf "reader"
        it "names the backend variable" $
            describeFailure WrongApduBackend
                `shouldSatisfy` T.isInfixOf "LPAC_APDU=pcsc"
        it "carries lpac's own reason" $
            describeFailure (LpacError "es10c_enable_profile" "x y")
                `shouldSatisfy` T.isInfixOf "x y"

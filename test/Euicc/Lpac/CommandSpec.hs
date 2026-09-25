module Euicc.Lpac.CommandSpec (spec) where

import Data.List (isInfixOf)
import Data.Text qualified as T
import Euicc.ActivationCode (DownloadTarget (..), mkSecret)
import Euicc.Lpac.Command
    ( Command (..)
    , commandArgs
    , lpacEnvironment
    )
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)
import Test.QuickCheck
    ( Gen
    , arbitrary
    , choose
    , elements
    , forAll
    , listOf
    , listOf1
    , oneof
    , property
    , (===)
    )

genNickname :: Gen T.Text
genNickname = T.pack <$> listOf1 (elements $ ['a' .. 'z'] <> ['0' .. '9'])

genCommand :: Gen Command
genCommand =
    oneof
        [ pure ReadChipInfo
        , pure ListProfiles
        , pure $ EnableProfile "8944476500001234567"
        , pure $ DeleteProfile "8944476500001234567"
        , NicknameProfile "8944476500001234567" <$> genNickname
        , pure ListNotifications
        , ProcessNotifications <$> listOf (choose (0, 1000))
        , DownloadProfile
            <$> (DownloadTarget "a.com" . mkSecret <$> genNickname <*> arbitrary)
            <*> oneof [pure Nothing, Just . mkSecret <$> genNickname]
        ]

download :: Command
download =
    DownloadProfile
        (DownloadTarget "a.com" (mkSecret "X-1") False)
        Nothing

spec :: Spec
spec = do
    describe "commandArgs" $ do
        it "reads chip info" $
            commandArgs ReadChipInfo `shouldBe` ["chip", "info"]
        it "lists profiles" $
            commandArgs ListProfiles `shouldBe` ["profile", "list"]
        it "enables by ICCID" $
            commandArgs (EnableProfile "894")
                `shouldBe` ["profile", "enable", "894"]
        it "deletes by ICCID" $
            commandArgs (DeleteProfile "894")
                `shouldBe` ["profile", "delete", "894"]
        it "lists notifications" $
            commandArgs ListNotifications
                `shouldBe` ["notification", "list"]
        it "sends notifications, removing the ones delivered" $
            commandArgs (ProcessNotifications [7, 8])
                `shouldBe` ["notification", "process", "-r", "7", "8"]
        it "downloads with address and matching ID" $
            commandArgs download
                `shouldBe` [ "profile"
                           , "download"
                           , "-s"
                           , "a.com"
                           , "-m"
                           , "X-1"
                           ]
        it "downloads with a confirmation code when one is given" $
            commandArgs
                ( DownloadProfile
                    (DownloadTarget "a.com" (mkSecret "X-1") True)
                    (Just $ mkSecret "C-9")
                )
                `shouldBe` [ "profile"
                           , "download"
                           , "-s"
                           , "a.com"
                           , "-m"
                           , "X-1"
                           , "-c"
                           , "C-9"
                           ]
        it "nicknames a profile by ICCID" $
            commandArgs (NicknameProfile "894" "holiday")
                `shouldBe` ["profile", "nickname", "894", "holiday"]
        it "never disables or removes without sending; deletes only by ICCID" $
            property $
                forAll genCommand $ \c ->
                    let verbs = take 2 $ commandArgs c
                        isDelete = case c of
                            DeleteProfile _ -> True
                            _ -> False
                    in  length verbs == 2
                            && all
                                (`notElem` verbs)
                                ["disable", "remove", "purge"]
                            && (("delete" `elem` verbs) == isDelete)
        it "never shows the matching ID" $
            show download `shouldSatisfy` (not . isInfixOf "X-1")
    describe "lpacEnvironment" $ do
        it "forces the PC/SC backend"
            $ property
            $ forAll
                ( elements
                    [[], [("LPAC_APDU", "mbim")], [("HOME", "/h")]]
                )
            $ \env ->
                lookup "LPAC_APDU" (lpacEnvironment env)
                    === Just "pcsc"
        it "keeps the rest of the environment" $
            lookup
                "HOME"
                (lpacEnvironment [("HOME", "/h"), ("LPAC_APDU", "at")])
                `shouldBe` Just "/h"
        it "sets the backend exactly once" $
            length
                ( filter ((== "LPAC_APDU") . fst) $
                    lpacEnvironment [("LPAC_APDU", "at")]
                )
                `shouldBe` 1

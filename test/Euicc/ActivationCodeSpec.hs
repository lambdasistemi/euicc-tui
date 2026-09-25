module Euicc.ActivationCodeSpec (spec) where

import Data.Either (isLeft)
import Data.Text (Text)
import Data.Text qualified as T
import Euicc.ActivationCode
    ( DownloadTarget (..)
    , mask
    , mkSecret
    , parseActivationCode
    , redact
    , resolveDownloadInput
    , revealSecret
    )
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)
import Test.QuickCheck
    ( Gen
    , choose
    , elements
    , forAll
    , listOf1
    , property
    , vectorOf
    , (===)
    )

genMatchingId :: Gen Text
genMatchingId =
    T.pack <$> listOf1 (elements $ ['A' .. 'Z'] <> ['0' .. '9'] <> "-")

genSmdp :: Gen Text
genSmdp = do
    n <- choose (1, 3)
    labels <- vectorOf n $ listOf1 $ elements ['a' .. 'z']
    pure $ T.intercalate "." $ map T.pack labels <> ["com"]

target :: DownloadTarget -> (Text, Text)
target DownloadTarget{..} = (targetSmdp, revealSecret targetMatchingId)

requiresConfirmation :: DownloadTarget -> Bool
requiresConfirmation = targetConfirmationRequired

spec :: Spec
spec = do
    describe "parseActivationCode" $ do
        it "splits a well-formed code into address and matching ID" $
            property $
                forAll genSmdp $ \smdp ->
                    forAll genMatchingId $ \mid ->
                        fmap
                            target
                            (parseActivationCode $ "LPA:1$" <> smdp <> "$" <> mid)
                            === Right (smdp, mid)
        it "ignores a trailing OID" $
            fmap target (parseActivationCode "LPA:1$a.com$X-1$1.2.3")
                `shouldBe` Right ("a.com", "X-1")
        it "parses a code that requires a confirmation code" $
            fmap
                requiresConfirmation
                (parseActivationCode "LPA:1$a.com$X-1$1.2.3$1")
                `shouldBe` Right True
        it "treats flag 0 as no confirmation needed" $
            fmap
                requiresConfirmation
                (parseActivationCode "LPA:1$a.com$X-1$1.2.3$0")
                `shouldBe` Right False
        it "needs no confirmation when no flag is given" $
            fmap requiresConfirmation (parseActivationCode "LPA:1$a.com$X-1")
                `shouldBe` Right False
        it "rejects a code with too many fields" $
            parseActivationCode "LPA:1$a.com$X-1$1.2.3$1$extra"
                `shouldSatisfy` isLeft
        it "rejects an unknown format version" $
            parseActivationCode "LPA:2$a.com$X-1" `shouldSatisfy` isLeft
        it "rejects a missing matching ID" $
            parseActivationCode "LPA:1$a.com" `shouldSatisfy` isLeft
        it "never shows the matching ID in its value" $
            property $
                forAll genMatchingId $ \suffix ->
                    let mid = "SECRET-" <> suffix
                        shown =
                            show $ parseActivationCode $ "LPA:1$a.com$" <> mid
                    in  not $ T.unpack mid `isInfixOfS` shown
    describe "resolveDownloadInput" $ do
        it "takes address and code from separate fields" $
            fmap target (resolveDownloadInput " a.com " " X-1 ")
                `shouldBe` Right ("a.com", "X-1")
        it "accepts an LPA string pasted in the code field" $
            fmap target (resolveDownloadInput "" "LPA:1$a.com$X-1")
                `shouldBe` Right ("a.com", "X-1")
        it "accepts an LPA string pasted in the address field" $
            fmap target (resolveDownloadInput "LPA:1$a.com$X-1" "")
                `shouldBe` Right ("a.com", "X-1")
        it "carries the confirmation requirement through the fields" $
            fmap
                requiresConfirmation
                (resolveDownloadInput "" "LPA:1$a.com$X-1$1.2.3$1")
                `shouldBe` Right True
        it "rejects empty fields" $ do
            resolveDownloadInput "" "X-1" `shouldSatisfy` isLeft
            resolveDownloadInput "a.com" "" `shouldSatisfy` isLeft
    describe "masking" $ do
        it "mask hides every character" $
            property $
                forAll genMatchingId $ \mid ->
                    mask mid === T.replicate (T.length mid) "*"
        it "redact removes every occurrence of the secret" $
            property $
                forAll genMatchingId $ \mid ->
                    let t = "error: " <> mid <> " refused " <> mid
                    in  not $ mid `T.isInfixOf` redact (mkSecret mid) t
        it "redact leaves other text alone" $
            redact (mkSecret "SECRET") "a SECRET b" `shouldBe` "a *** b"
  where
    isInfixOfS a b = T.pack a `T.isInfixOf` T.pack b

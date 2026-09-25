module Euicc.QrSpec (spec) where

import Data.Text qualified as T
import Euicc.ActivationCode
    ( DownloadTarget (..)
    , revealSecret
    )
import Euicc.Lpac.Output (LpacFailure (..), describeFailure)
import Euicc.Qr (decodeQrFile)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

{- | The synthetic QR images recorded in test/fixtures, generated
with qrencode. None of them carries a real activation code.
-}
image :: FilePath -> FilePath
image name = "test/fixtures/" <> name

spec :: Spec
spec = do
    describe "decodeQrFile" $ do
        it "decodes an activation code from a QR image" $ do
            r <- decodeQrFile $ image "qr-lpa-ok.png"
            fmap targetSmdpOf r `shouldBe` Right "qr-smdp.example.org"
            fmap (revealSecret . matchingIdOf) r
                `shouldBe` Right "QR-MATCH-7X"
        it "decodes the confirmation flag" $ do
            r <- decodeQrFile $ image "qr-lpa-confirm.png"
            fmap confirmationOf r `shouldBe` Right True
        it "refuses a QR code that is not an activation code" $ do
            r <- decodeQrFile $ image "qr-not-lpa.png"
            r `shouldSatisfy` isQrDecode
            let msg = either describeFailure (const "") r
            msg `shouldSatisfy` T.isInfixOf "activation code"
        it "reports an image with no QR code" $ do
            r <- decodeQrFile $ image "qr-none.png"
            r `shouldSatisfy` isQrDecode
            let msg = either describeFailure (const "") r
            msg `shouldSatisfy` T.isInfixOf "no QR code"
        it "reports a missing file" $ do
            r <- decodeQrFile $ image "qr-absent.png"
            r `shouldSatisfy` isQrDecode
        it "never shows the matching ID in a failure" $ do
            r <- decodeQrFile $ image "qr-not-lpa.png"
            let shown =
                    either
                        (\e -> T.pack (show e) <> describeFailure e)
                        (T.pack . show)
                        r
            shown `shouldSatisfy` (not . T.isInfixOf "QR-MATCH")
  where
    targetSmdpOf = targetSmdp
    matchingIdOf = targetMatchingId
    confirmationOf = targetConfirmationRequired
    isQrDecode (Left (QrDecode _)) = True
    isQrDecode _ = False

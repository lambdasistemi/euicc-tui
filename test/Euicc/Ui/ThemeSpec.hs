module Euicc.Ui.ThemeSpec (spec) where

import Euicc.Ui.Theme (Theme (..), themeOfBackground)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "themeOfBackground" $ do
    it "reads a dark background" $
        themeOfBackground "\ESC]11;rgb:1d1d/1d1d/2020\ESC\\"
            `shouldBe` Just Dark
    it "reads a light background" $
        themeOfBackground "\ESC]11;rgb:ffff/ffff/ffff\a"
            `shouldBe` Just Light
    it "reads two-digit channels" $
        themeOfBackground "\ESC]11;rgb:fa/fa/fa\a" `shouldBe` Just Light
    it "rejects a reply without a colour" $
        themeOfBackground "" `shouldBe` Nothing

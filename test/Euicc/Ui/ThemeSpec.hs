module Euicc.Ui.ThemeSpec (spec) where

import Euicc.Ui.Theme
    ( Theme (..)
    , reportedTheme
    , themeOfBackground
    , themeReports
    )
import Graphics.Vty (Event (..), Key (..))
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
    describe "reportedTheme" $ do
        it "reads the terminal's dark and light reports" $
            [reportedTheme e | (_, _, e) <- themeReports]
                `shouldBe` [Just Dark, Just Light]
        it "ignores ordinary keys" $
            reportedTheme (EvKey (KChar 't') []) `shouldBe` Nothing
    describe "themeOfBackground" $ do
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

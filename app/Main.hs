module Main (main) where

import Euicc.Lpac.Process (processRunner)
import Euicc.Ui.App (runApp)

main :: IO ()
main = processRunner >>= runApp

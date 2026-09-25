module Main (main) where

import Data.Version (showVersion)
import Euicc.Lpac.Process (processRunner)
import Euicc.Ui.App (runApp)
import Paths_euicc_tui (version)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

main :: IO ()
main =
    getArgs >>= \case
        [] -> processRunner >>= runApp
        ["--version"] -> putStrLn $ "euicc-tui " <> showVersion version
        ["--help"] -> putStr usage
        _ -> hPutStrLn stderr usage >> exitFailure

usage :: String
usage =
    unlines
        [ "Usage: euicc-tui [--version | --help]"
        , ""
        , "Terminal UI for the eSIM profiles on a removable eUICC card,"
        , "through lpac and a PC/SC reader. Press ? inside for the keys."
        ]

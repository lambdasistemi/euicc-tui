module Euicc.Lpac.Process
    ( processRunner
    ) where

-- \|
-- Module      : Euicc.Lpac.Process
-- Description : Run the real lpac executable
-- Copyright   : (c) Paolo Veronelli, 2026
-- License     : Apache-2.0
--
-- The production 'LpacRunner': runs @lpac@ from @PATH@ with
-- @LPAC_APDU=pcsc@, stdin closed, and captures stdout, stderr and the
-- exit code. A failure to start the process is reported as output, not
-- thrown.

import Control.Exception (IOException, try)
import Data.ByteString.Lazy qualified as BL
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Euicc.Job (LpacRunner (..))
import Euicc.Lpac.Command (Command, commandArgs, lpacEnvironment)
import Euicc.Lpac.Output (RawOutput (..))
import System.Environment (getEnvironment)
import System.Exit (ExitCode (..))
import System.Process.Typed
    ( nullStream
    , proc
    , readProcess
    , setEnv
    , setStdin
    )

-- | A runner for the @lpac@ found on @PATH@.
processRunner :: IO LpacRunner
processRunner = do
    env <- lpacEnvironment <$> getEnvironment
    pure $ LpacRunner $ runLpacWith env

runLpacWith :: [(String, String)] -> Command -> IO RawOutput
runLpacWith env command = do
    r <- try $ readProcess config
    pure $ case r of
        Right (code, out, err) ->
            RawOutput
                { rawExit = code
                , rawStdout = BL.toStrict out
                , rawStderr = BL.toStrict err
                }
        Left (e :: IOException) ->
            RawOutput
                { rawExit = ExitFailure 127
                , rawStdout = ""
                , rawStderr =
                    encodeUtf8 $ "cannot run lpac: " <> T.pack (show e)
                }
  where
    config =
        setStdin nullStream
            $ setEnv env
            $ proc "lpac"
            $ commandArgs command

module Fixtures
    ( fixture
    , fixtureOk
    ) where

-- \|
-- Module      : Fixtures
-- Description : Load recorded lpac runs from test/fixtures
-- Copyright   : (c) Paolo Veronelli, 2026
-- License     : Apache-2.0

import Control.Exception (IOException, try)
import Data.ByteString qualified as BS
import Data.Either (fromRight)
import Euicc.Lpac.Output (RawOutput (..))
import System.Exit (ExitCode (..))

{- | A recorded run: @test/fixtures/<name>.stdout@ and, when present,
@<name>.stderr@, with the given exit code.
-}
fixture :: ExitCode -> FilePath -> IO RawOutput
fixture code name = do
    out <- BS.readFile $ base <> ".stdout"
    err <- fromRight "" <$> readOptional (base <> ".stderr")
    pure RawOutput{rawExit = code, rawStdout = out, rawStderr = err}
  where
    base = "test/fixtures/" <> name
    readOptional :: FilePath -> IO (Either IOException BS.ByteString)
    readOptional = try . BS.readFile

-- | A recorded run that exited successfully.
fixtureOk :: FilePath -> IO RawOutput
fixtureOk = fixture ExitSuccess

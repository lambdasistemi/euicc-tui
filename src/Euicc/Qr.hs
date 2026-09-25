module Euicc.Qr
    ( decodeQrFile
    ) where

-- \|
-- Module      : Euicc.Qr
-- Description : Read an activation code from a QR image
-- Copyright   : (c) Paolo Veronelli, 2026
-- License     : Apache-2.0
--
-- Providers deliver travel eSIMs as a QR image (a screenshot or the
-- picture saved from the purchase email). This module decodes one
-- image file with @zbarimg@ and parses the result as an activation
-- code. The image never reaches @lpac@, and the matching ID inside a
-- decoded code stays in a 'Euicc.ActivationCode.Secret'.

import Control.Exception (IOException, try)
import Data.ByteString.Lazy qualified as BL
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import Euicc.ActivationCode (DownloadTarget, parseActivationCode)
import Euicc.Lpac.Output (LpacFailure (..))
import System.Exit (ExitCode (..))
import System.Process.Typed
    ( nullStream
    , proc
    , readProcess
    , setStdin
    )

{- | Decode the QR code in an image file and parse it as an
activation code. Fails when @zbarimg@ is missing, when the image
holds no QR code, or when the decoded text is not an @LPA:1$...@
code.
-}
decodeQrFile :: FilePath -> IO (Either LpacFailure DownloadTarget)
decodeQrFile path = do
    r <- try $ readProcess config
    pure $ case r of
        Left (_ :: IOException) ->
            Left $ QrDecode "zbarimg is not available. Install zbar."
        Right (ExitSuccess, out, _) ->
            parse $ firstLine $ decodeUtf8With lenientDecode $ BL.toStrict out
        Right (ExitFailure 4, _, _) ->
            Left $ QrDecode "no QR code found in this image."
        Right (ExitFailure _, _, err) ->
            Left
                $ QrDecode
                $ T.strip
                $ decodeUtf8With lenientDecode
                $ BL.toStrict err
  where
    config =
        setStdin nullStream $ proc "zbarimg" ["--raw", path]
    firstLine = T.takeWhile (/= '\n') . T.strip
    parse text = case parseActivationCode text of
        Right target -> Right target
        Left reason ->
            Left
                $ QrDecode
                $ "the QR code does not hold an activation code ("
                    <> reason
                    <> ")."

module Euicc.ActivationCode
    ( Secret
    , mkSecret
    , revealSecret
    , redact
    , mask
    , DownloadTarget (..)
    , parseActivationCode
    , resolveDownloadInput
    ) where

-- \|
-- Module      : Euicc.ActivationCode
-- Description : Activation codes and the secret they carry
-- Copyright   : (c) Paolo Veronelli, 2026
-- License     : Apache-2.0
--
-- An eSIM activation code (SGP.22 section 4.1) has the shape
-- @LPA:1$<SM-DP+ address>$<matching ID>[$<OID>$<confirmation flag>]@.
-- The matching ID authorises a download, so it is held in a 'Secret'
-- whose 'Show' instance never prints it, and every text that may have
-- seen it passes through 'redact' before reaching the screen.

import Control.Monad (unless, when)
import Data.Text (Text)
import Data.Text qualified as T

-- | A value that must not be shown or logged.
newtype Secret = Secret Text
    deriving stock (Eq)

instance Show Secret where
    show _ = "<redacted>"

-- | Wrap a sensitive value.
mkSecret :: Text -> Secret
mkSecret = Secret

-- | The wrapped value, for passing to @lpac@ only.
revealSecret :: Secret -> Text
revealSecret (Secret t) = t

-- | Where to download a profile from.
data DownloadTarget = DownloadTarget
    { targetSmdp :: Text
    -- ^ SM-DP+ address
    , targetMatchingId :: Secret
    -- ^ matching ID (the activation code proper)
    , targetConfirmationRequired :: Bool
    -- ^ the activation code asks for a confirmation code
    -- (SGP.22 flag @1@), which the operator must type in
    }
    deriving stock (Eq, Show)

-- | Replace every occurrence of the secret in a text.
redact :: Secret -> Text -> Text
redact (Secret s) t
    | T.null s = t
    | otherwise = T.replace s "***" t

-- | What a masked input field shows: one @*@ per character.
mask :: Text -> Text
mask t = T.replicate (T.length t) "*"

-- | Parse a full @LPA:1$...@ activation code.
parseActivationCode :: Text -> Either Text DownloadTarget
parseActivationCode input = do
    body <-
        maybe (Left "an activation code starts with LPA:") Right $
            T.stripPrefix "LPA:" (T.strip input)
    case T.splitOn "$" body of
        (format : smdp : matchingId : rest) -> do
            unless (format == "1") $
                Left "unsupported activation code format"
            when (T.null smdp) $ Left "the SM-DP+ address is empty"
            when (T.null matchingId) $ Left "the matching ID is empty"
            confirmationRequired <- flagOf rest
            pure
                DownloadTarget
                    { targetSmdp = smdp
                    , targetMatchingId = Secret matchingId
                    , targetConfirmationRequired = confirmationRequired
                    }
        _ -> Left "an activation code has the form LPA:1$<address>$<code>"
  where
    -- The fields after the matching ID are the optional OID of the
    -- operator asking for a confirmation, then the flag itself.
    flagOf = \case
        [] -> Right False
        [_oid] -> Right False
        [_oid, flag] -> Right (flag == "1")
        _ -> Left "unsupported activation code format"

{- | Build a target from the two form fields. Either field may hold a
pasted @LPA:1$...@ string, which then supplies both parts.
-}
resolveDownloadInput
    :: Text
    -- ^ SM-DP+ address field
    -> Text
    -- ^ activation code field
    -> Either Text DownloadTarget
resolveDownloadInput smdpField codeField
    | isLpa code = parseActivationCode code
    | isLpa smdp = parseActivationCode smdp
    | T.null smdp = Left "the SM-DP+ address is empty"
    | T.null code = Left "the activation code is empty"
    | otherwise =
        Right
            DownloadTarget
                { targetSmdp = smdp
                , targetMatchingId = Secret code
                , targetConfirmationRequired = False
                }
  where
    smdp = T.strip smdpField
    code = T.strip codeField
    isLpa = T.isPrefixOf "LPA:"

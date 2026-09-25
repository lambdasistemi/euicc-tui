module Euicc.Ui.Theme
    ( Theme (..)
    , detectTheme
    , themeOfBackground
    , followChanges
    , stopFollowing
    , themeReports
    , reportedTheme
    ) where

-- \|
-- Module      : Euicc.Ui.Theme
-- Description : Whether the terminal is light or dark
-- Copyright   : (c) Paolo Veronelli, 2026
-- License     : Apache-2.0
--
-- The screen paints its own backgrounds, so it has to know what the
-- terminal's is. @EUICC_TUI_THEME=light|dark@ decides; otherwise the
-- terminal is asked for its background colour (OSC 11), and a
-- terminal that does not answer gets the dark palette.

import Control.Exception (IOException, bracket, try)
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.ByteString.Char8 qualified as BC
import Data.Char (isHexDigit, toLower)
import Data.Either (fromRight)
import Graphics.Vty (Event (..), Key (..))
import Numeric (readHex)
import System.Environment (lookupEnv)
import System.Posix.IO
    ( OpenMode (..)
    , closeFd
    , defaultFileFlags
    , openFd
    )
import System.Posix.IO.ByteString (fdRead, fdWrite)
import System.Posix.Terminal
    ( TerminalAttributes
    , TerminalMode (..)
    , TerminalState (..)
    , getTerminalAttributes
    , setTerminalAttributes
    , withMinInput
    , withTime
    , withoutMode
    )
import System.Posix.Types (Fd)

data Theme = Light | Dark
    deriving stock (Eq, Show)

{- | The theme to draw with, and whether to follow the terminal's
changes: not when @EUICC_TUI_THEME@ forces one.
-}
detectTheme :: IO (Theme, Bool)
detectTheme = do
    forced <- lookupEnv "EUICC_TUI_THEME"
    case map toLower <$> forced of
        Just "light" -> pure (Light, False)
        Just "dark" -> pure (Dark, False)
        _ ->
            (,True) <$> do
                answer <- try @IOException askBackground
                pure $ case answer of
                    Right reply
                        | Just t <- themeOfBackground reply -> t
                    _ -> Dark
  where
    askBackground :: IO ByteString
    askBackground =
        bracket (openFd "/dev/tty" ReadWrite defaultFileFlags) closeFd $
            \fd -> bracket (getTerminalAttributes fd) (restore fd) $
                \attrs -> do
                    setTerminalAttributes fd (raw attrs) Immediately
                    _ <- fdWrite fd "\ESC]11;?\ESC\\"
                    collect fd (5 :: Int) ""
    restore fd attrs = setTerminalAttributes fd attrs Immediately
    raw :: TerminalAttributes -> TerminalAttributes
    raw a =
        withTime
            (withMinInput (withoutMode (withoutMode a ProcessInput) EnableEcho) 0)
            1
    collect :: Fd -> Int -> ByteString -> IO ByteString
    collect fd tries acc
        | tries == 0 || complete acc = pure acc
        | otherwise = do
            chunk <- fromRight "" <$> readSome fd
            if B.null chunk
                then collect fd (tries - 1) acc
                else collect fd tries (acc <> chunk)
    readSome :: Fd -> IO (Either IOException ByteString)
    readSome fd = try $ fdRead fd 64
    complete bs = BC.elem '\a' bs || "\ESC\\" `B.isInfixOf` bs

{- | The theme matching a terminal's answer to the background colour
query, @ESC ] 11 ; rgb:RRRR/GGGG/BBBB@: dark below half luminance.
-}
themeOfBackground :: ByteString -> Maybe Theme
themeOfBackground reply = case B.breakSubstring "rgb:" reply of
    (_, rest)
        | B.null rest -> Nothing
        | otherwise -> case map channel $ take 3 $ BC.split '/' $ B.drop 4 rest of
            [Just r, Just g, Just b]
                | 0.2126 * r + 0.7152 * g + 0.0722 * b < (0.5 :: Double) ->
                    Just Dark
                | otherwise -> Just Light
            _ -> Nothing
  where
    channel c = case BC.unpack $ BC.takeWhile isHexDigit c of
        [] -> Nothing
        ds -> case readHex ds of
            [(v, "")] -> Just $ fromInteger v / (16 ^ length ds - 1)
            _ -> Nothing

{- | Ask the terminal to report light/dark changes (DEC mode 2031),
and the current one. Terminals without the mode ignore both.
-}
followChanges :: ByteString
followChanges = "\ESC[?2031h\ESC[?996n"

-- | Stop the reports.
stopFollowing :: ByteString
stopFollowing = "\ESC[?2031l"

{- | The reports, as input the terminal layer turns into events: the
function keys 9971 (dark) and 9972 (light), which no keyboard has.
-}
themeReports :: [(Maybe String, String, Event)]
themeReports =
    [ (Nothing, "\ESC[?997;1n", EvKey (KFun 9971) [])
    , (Nothing, "\ESC[?997;2n", EvKey (KFun 9972) [])
    ]

-- | The theme a report event announces.
reportedTheme :: Event -> Maybe Theme
reportedTheme = \case
    EvKey (KFun 9971) [] -> Just Dark
    EvKey (KFun 9972) [] -> Just Light
    _ -> Nothing

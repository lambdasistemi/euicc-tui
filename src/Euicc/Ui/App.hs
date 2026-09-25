module Euicc.Ui.App
    ( runApp
    ) where

-- \|
-- Module      : Euicc.Ui.App
-- Description : The brick terminal front end
-- Copyright   : (c) Paolo Veronelli, 2026
-- License     : Apache-2.0
--
-- Draws 'State' and feeds key presses to 'handleKey'. Jobs run on a
-- separate thread and report back through a brick event channel, so a
-- slow @lpac@ call never freezes the screen.

import Brick
    ( App (..)
    , AttrMap
    , AttrName
    , BrickEvent (..)
    , EventM
    , Padding (..)
    , Widget
    , attrMap
    , attrName
    , clickable
    , customMain
    , emptyWidget
    , fg
    , get
    , getVtyHandle
    , hBox
    , hLimit
    , halt
    , neverShowCursor
    , on
    , overrideAttr
    , padBottom
    , padLeft
    , padLeftRight
    , padRight
    , padTop
    , padTopBottom
    , put
    , txt
    , txtWrap
    , vBox
    , withAttr
    , withBorderStyle
    )
import Brick.BChan (newBChan, writeBChan)
import Brick.Widgets.Border (borderAttr, hBorder)
import Brick.Widgets.Border.Style (unicodeRounded)
import Brick.Widgets.Center (center, hCenter)
import Brick.Widgets.Dialog (dialog, dialogAttr, renderDialog)
import Control.Concurrent (forkIO)
import Control.Monad (void, when)
import Control.Monad.IO.Class (liftIO)
import Data.Char (toLower, toUpper)
import Data.Foldable (traverse_)
import Data.List (find)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Euicc.Job
    ( Job (..)
    , JobResult
    , LpacRunner
    , Snapshot (..)
    , jobLabel
    , runJob
    )
import Euicc.Lpac.Output
    ( ChipInfo (..)
    , Notification (..)
    , Profile (..)
    , ProfileState (..)
    , describeFailure
    , profileLabel
    )
import Euicc.Ui.State
    ( Browser (..)
    , Click (..)
    , Field (..)
    , Form (..)
    , State (..)
    , Status (..)
    , Step (..)
    , View (..)
    , Wizard (..)
    , WizardPhase (..)
    , codeDisplay
    , confirmDisplay
    , deleteCheck
    , finishJob
    , handleClick
    , handleKey
    , smdpDisplay
    , start
    , typing
    )
import Graphics.Vty qualified as V
import Graphics.Vty.CrossPlatform (mkVty)
import System.FilePath (takeExtension)

-- | A job finished on the worker thread.
newtype AppEvent = JobDone JobResult

-- | The widgets a click can land on.
data Name
    = ProfileRow Int
    | NotificationRow Int
    | TabOf View
    | PickerEntry Int
    | FieldOf Field
    | KeyButton V.Key
    deriving stock (Eq, Ord, Show)

-- | Run the UI until the user quits.
runApp :: LpacRunner -> IO ()
runApp runner = do
    chan <- newBChan 8
    let launch job =
            void $ forkIO $ runJob runner job >>= writeBChan chan . JobDone
        (s0, j0) = start
    launch j0
    let buildVty = mkVty V.defaultConfig
    vty <- buildVty
    void $ customMain vty buildVty (Just chan) (app launch) s0

app :: (Job -> IO ()) -> App State AppEvent Name
app launch =
    App
        { appDraw = draw
        , appChooseCursor = neverShowCursor
        , appHandleEvent = handleEvent launch
        , appStartEvent = enableMouse
        , appAttrMap = const attributes
        }

-- | Ask the terminal for mouse events, when it can report them.
enableMouse :: EventM Name State ()
enableMouse = do
    output <- V.outputIface <$> getVtyHandle
    when (V.supportsMode output V.Mouse)
        $ liftIO
        $ V.setMode output V.Mouse True

handleEvent
    :: (Job -> IO ()) -> BrickEvent Name AppEvent -> EventM Name State ()
handleEvent launch = \case
    VtyEvent (V.EvKey key mods) -> step $ handleKey key mods
    MouseDown name button _ _ -> case clickOf name button of
        Just c -> step $ handleClick c
        Nothing -> pure ()
    AppEvent (JobDone r) -> do
        s <- get
        let (s', job) = finishJob r s
        put s'
        liftIO $ traverse_ launch job
    _ -> pure ()
  where
    step f = do
        s <- get
        case f s of
            Halt -> halt
            Continue s' job -> do
                put s'
                liftIO $ traverse_ launch job

-- | What a mouse button press on a named widget means.
clickOf :: Name -> V.Button -> Maybe Click
clickOf name = \case
    V.BScrollUp -> Just WheelUp
    V.BScrollDown -> Just WheelDown
    V.BLeft -> Just $ case name of
        ProfileRow i -> ClickProfile i
        NotificationRow i -> ClickNotification i
        TabOf v -> ClickTab v
        PickerEntry i -> ClickPicker i
        FieldOf f -> ClickField f
        KeyButton k -> ClickKey k
    _ -> Nothing

-- Attributes -------------------------------------------------------

barAttr
    , tabAttr
    , tabActiveAttr
    , columnAttr
    , plainAttr
    , stripeAttr
    , selectedAttr
    , dimAttr
    , keyAttr
    , buttonAttr
    , buttonKeyAttr
    , titleAttr
    , dangerAttr
    , dangerBorderAttr
    , focusLabelAttr
    , inputAttr
    , inputFocusAttr
    , dirAttr
    , failureAttr
    , infoAttr
    , busyAttr
        :: AttrName
barAttr = attrName "bar"
tabAttr = attrName "tab"
tabActiveAttr = attrName "tabActive"
columnAttr = attrName "column"
plainAttr = attrName "plain"
stripeAttr = attrName "stripe"
selectedAttr = attrName "selected"
dimAttr = attrName "dim"
keyAttr = attrName "key"
buttonAttr = attrName "button"
buttonKeyAttr = buttonAttr <> attrName "key"
titleAttr = attrName "title"
dangerAttr = attrName "danger"
dangerBorderAttr = attrName "dangerBorder"
focusLabelAttr = attrName "focusLabel"
inputAttr = attrName "input"
inputFocusAttr = attrName "inputFocus"
dirAttr = attrName "dir"
failureAttr = attrName "failure"
infoAttr = attrName "info"
busyAttr = attrName "busy"

-- | The background of every other table row.
stripe :: V.Color
stripe = V.rgbColor (0xff :: Int) 0xff 0xd7

-- | The background of the selected row and the active tab.
selection :: V.Color
selection = V.rgbColor (0xaf :: Int) 0xff 0xff

-- | The background of the other table rows.
paper :: V.Color
paper = V.rgbColor (0xff :: Int) 0xff 0xff

attributes :: AttrMap
attributes =
    attrMap
        V.defAttr
        [ (barAttr, V.white `on` V.blue `V.withStyle` V.bold)
        , (tabAttr, fg V.brightBlack)
        , (tabActiveAttr, V.black `on` selection `V.withStyle` V.bold)
        , (columnAttr, V.defAttr `V.withStyle` V.bold)
        , (plainAttr, V.black `on` paper)
        , (stripeAttr, V.black `on` stripe)
        , (selectedAttr, V.black `on` selection `V.withStyle` V.bold)
        , (dimAttr, fg $ V.rgbColor (0x5f :: Int) 0x5f 0x5f)
        , (keyAttr, fg V.blue `V.withStyle` V.bold)
        , (buttonAttr, V.black `on` V.rgbColor (0xaf :: Int) 0xd7 0xff)
        , (buttonKeyAttr, fg V.blue `V.withStyle` V.bold)
        , (dialogAttr, V.black `on` V.rgbColor (0xee :: Int) 0xee 0xee)
        , (borderAttr, fg V.brightBlack)
        , (dangerBorderAttr, fg V.red)
        , (focusLabelAttr, fg V.blue `V.withStyle` V.bold)
        , (titleAttr, V.defAttr `V.withStyle` V.bold)
        , (dangerAttr, fg V.red `V.withStyle` V.bold)
        , (inputAttr, V.black `on` V.rgbColor (0xd0 :: Int) 0xd0 0xd0)
        , (inputFocusAttr, V.black `on` V.white)
        , (dirAttr, fg V.blue `V.withStyle` V.bold)
        , (failureAttr, fg V.red `V.withStyle` V.bold)
        , (infoAttr, fg V.cyan)
        , (busyAttr, fg V.yellow `V.withStyle` V.bold)
        ]

-- Layout -----------------------------------------------------------

draw :: State -> [Widget Name]
draw s =
    [ helpLayer s
    , browserLayer s
    , deleteLayer s
    , confirmLayer s
    , nicknameLayer s
    , mainLayer s
    ]

mainLayer :: State -> Widget Name
mainLayer s =
    vBox
        [ topBar s
        , txt " "
        , tabs s
        , hBorder
        , padBottom Max $ padTop (Pad 1) $ body s
        , hBorder
        , bottomLine s
        ]

topBar :: State -> Widget Name
topBar s =
    withAttr barAttr $
        hBox
            [ txt " euicc-tui "
            , padLeft Max $ txt $ case snapshotOf s of
                Just snap ->
                    let ChipInfo{..} = snapChip snap
                    in  "EID "
                            <> chipEid
                            <> "  │  "
                            <> maybe "free memory unknown" formatBytes chipFreeMemory
                            <> " "
                Nothing -> "no card read "
            ]

formatBytes :: Integer -> Text
formatBytes n = T.pack (show $ n `div` 1024) <> " KiB free"

tabs :: State -> Widget Name
tabs s =
    padLeftRight 1 $
        hBox
            [ clickable (TabOf ProfilesView) $ tab onProfiles " Profiles "
            , txt " "
            , clickable (TabOf NotificationsView)
                $ tab (stView s == NotificationsView)
                $ " Notifications"
                    <> maybe "" (\n -> " (" <> T.pack (show n) <> ")") pending
                    <> " "
            , padLeft Max $ case stView s of
                DownloadView -> withAttr titleAttr $ txt "Download a profile"
                WizardView -> withAttr titleAttr $ txt "Install a plan"
                _ -> emptyWidget
            ]
  where
    onProfiles = stView s `elem` [ProfilesView, DownloadView, WizardView]
    pending = case length . snapNotifications <$> snapshotOf s of
        Just n | n > 0 -> Just n
        _ -> Nothing
    tab active = withAttr (if active then tabActiveAttr else tabAttr) . txt

body :: State -> Widget Name
body s = case stCard s of
    Nothing -> center $ withAttr dimAttr $ txt "Reading the card..."
    Just (Left f) ->
        panel
            "The card could not be read"
            [ withAttr failureAttr $ txtWrap $ describeFailure f
            , txt " "
            , hints [("r", "try again"), ("q", "quit")]
            ]
    Just (Right snap) -> case stView s of
        ProfilesView -> profilesTable s snap
        NotificationsView -> notificationsTable s snap
        DownloadView ->
            formPanel
                (stForm s)
                [ "A full LPA:1$... string may be pasted into either"
                , "field. The activation code is never shown."
                ]
        WizardView -> wizardBody s

-- Tables -----------------------------------------------------------

-- | A table row: fixed-width cells, padded to the full line.
row :: [(Int, Text)] -> Widget Name
row cells = padRight Max $ hBox $ map cell cells
  where
    cell (w, t) = hLimit w $ padRight Max $ txt $ fill $ clip w t
    clip w t
        | T.length t < w = t
        | otherwise = T.take (w - 2) t <> "…"
    fill t = if T.null t then " " else t

headerRow :: [(Int, Text)] -> Widget Name
headerRow = withAttr columnAttr . row

-- | The attribute of a table row: selected, else striped.
rowAttr :: Bool -> Int -> AttrName
rowAttr selected i
    | selected = selectedAttr
    | odd i = stripeAttr
    | otherwise = plainAttr

profilesTable :: State -> Snapshot -> Widget Name
profilesTable s snap = case snapProfiles snap of
    [] ->
        center $
            vBox
                [ hCenter $ withAttr dimAttr $ txt "No profiles on this card."
                , txt " "
                , hCenter $ hints [("g", "install a plan")]
                ]
    ps ->
        padLeftRight 1
            $ vBox
            $ headerRow (zip widths ["", "Name", "Provider", "ICCID", "State"])
                : zipWith profileRow [0 ..] ps
  where
    widths = [3, 30, 22, 23, 10]
    profileRow :: Int -> Profile -> Widget Name
    profileRow i p =
        clickable (ProfileRow i)
            $ withAttr (rowAttr (i == stProfileCursor s) i)
            $ row
            $ zip
                widths
                [ if enabled p then " ●" else ""
                , profileLabel p
                , fromMaybe "" $ profileProvider p
                , profileIccid p
                , stateText $ profileState p
                ]
    enabled p = profileState p == Enabled
    stateText = \case
        Enabled -> "enabled"
        Disabled -> "disabled"
        OtherState t -> t

notificationsTable :: State -> Snapshot -> Widget Name
notificationsTable s snap = case snapNotifications snap of
    [] ->
        center
            $ withAttr dimAttr
            $ txt "No pending notifications. The operators are up to date."
    ns ->
        padLeftRight 1
            $ vBox
            $ [ withAttr dimAttr $
                    txtWrap
                        "Each change to a profile leaves a notification for \
                        \its operator. Sending needs the network."
              , txt " "
              , headerRow (zip widths ["Seq", "Operation", "Profile", "Server"])
              ]
                <> zipWith notificationRow [0 ..] ns
  where
    widths = [6, 12, 30, 40]
    notificationRow :: Int -> Notification -> Widget Name
    notificationRow i Notification{..} =
        clickable (NotificationRow i)
            $ withAttr (rowAttr (i == stNotificationCursor s) i)
            $ row
            $ zip
                widths
                [ T.pack $ show notificationSeq
                , notificationOperation
                , maybe "" profileOf notificationIccid
                , fromMaybe "" notificationAddress
                ]
    profileOf iccid =
        maybe iccid profileLabel
            $ find ((== iccid) . profileIccid)
            $ snapProfiles snap

-- Panels and forms -------------------------------------------------

-- | A titled, rounded box of bounded width.
panel :: Text -> [Widget Name] -> Widget Name
panel = panelWith False

panelWith :: Bool -> Text -> [Widget Name] -> Widget Name
panelWith danger title contents =
    (if danger then overrideAttr borderAttr dangerBorderAttr else id)
        $ withBorderStyle unicodeRounded
        $ renderDialog
            ( dialog
                ( Just
                    $ withAttr (if danger then dangerAttr else titleAttr)
                    $ txt
                    $ " " <> title <> " "
                )
                Nothing
                66
            )
        $ padLeftRight 2
        $ padTopBottom 1
        $ vBox contents

-- | A text input box, with a caret when it has the focus.
input :: Bool -> Int -> Text -> Widget Name
input focused w value =
    withAttr (if focused then inputFocusAttr else inputAttr)
        $ hLimit w
        $ padRight Max
        $ txt
        $ " " <> T.takeEnd (w - 3) value <> (if focused then "▏" else " ")

-- | An input box showing a hint while it is empty.
inputOr :: Text -> Bool -> Int -> Text -> Widget Name
inputOr placeholder focused w value
    | T.null value =
        withAttr (if focused then inputFocusAttr else inputAttr)
            $ hLimit w
            $ padRight Max
            $ txt
            $ " " <> placeholder
    | otherwise = input focused w value

-- | A labelled form field.
field :: Bool -> Text -> Widget Name -> Widget Name
field focused label widget =
    hBox
        [ withAttr (if focused then focusLabelAttr else dimAttr)
            $ txt
            $ (if focused then "› " else "  ") <> label
        , widget
        ]

-- | The source form shared by the download view and the guided install.
formPanel :: Form -> [Text] -> Widget Name
formPanel form notes =
    panel "Where does the plan come from?" $
        [ clickable (FieldOf QrField)
            $ field (focused QrField) "QR image         "
            $ inputOr
                "enter to browse, or type a path"
                (focused QrField)
                40
                (formQr form)
        , txt " "
        , clickable (FieldOf SmdpField)
            $ field (focused SmdpField) "SM-DP+ address   "
            $ input (focused SmdpField) 40 (smdpDisplay form)
        , txt " "
        , clickable (FieldOf CodeField)
            $ field (focused CodeField) "Activation code  "
            $ input (focused CodeField) 40 (codeDisplay form)
        , txt " "
        ]
            <> map (withAttr dimAttr . txt) notes
  where
    focused f = formFocus form == f

wizardBody :: State -> Widget Name
wizardBody s = case wzPhase w of
    WzSource ->
        formPanel
            form
            [ "Pick the QR image from the provider's email, or type"
            , "the address and code by hand. The code is never shown."
            ]
    WzReady
        | Just (Download _ _) <- stBusy s ->
            panel
                "Installing"
                [ withAttr busyAttr $ txt "Downloading the plan onto the card..."
                , txt " "
                , withAttr dimAttr $
                    txt "This takes a few seconds; the list follows."
                ]
        | otherwise ->
            panel
                "QR code read"
                [ summary "SM-DP+ address" $ smdpDisplay form
                , summary "Activation code" $ codeDisplay form
                , txt " "
                , txt "Install this plan on the card?"
                , txt " "
                , hints [("enter", "install"), ("esc", "cancel")]
                ]
    WzConfirm ->
        panel
            "Confirmation code"
            [ txtWrap
                "This activation code asks for a confirmation code. \
                \The provider sent it separately; it is never shown."
            , txt " "
            , field True "Confirmation code  " $ input True 30 $ confirmDisplay w
            , txt " "
            , hints [("enter", "install"), ("esc", "cancel")]
            ]
  where
    w = fromMaybe (Wizard WzSource [] "") $ stWizard s
    form = stForm s
    summary label value =
        hBox
            [ withAttr dimAttr $ hLimit 18 $ padRight Max $ txt label
            , withAttr titleAttr $ txt value
            ]

-- Status and help --------------------------------------------------

-- | The status on the left; on the right, how to get help.
bottomLine :: State -> Widget Name
bottomLine s =
    padLeftRight 1 $
        hBox
            [ padRight Max status
            , if typing s
                then
                    hints
                        ( [("tab", "field") | inForm]
                            <> [("enter", "ok"), ("esc", "cancel"), ("?", "help")]
                        )
                else hints [("?", "help")]
            ]
  where
    inForm = case (stNicknameEdit s, stDelete s) of
        (Nothing, Nothing) -> True
        _ -> False
    status = case (stBusy s, stStatus s) of
        (Just job, _) ->
            withAttr busyAttr $ txt $ "⟳ " <> capital (jobLabel job) <> "..."
        (Nothing, Just (Info t)) -> withAttr infoAttr $ txtWrap $ "• " <> t
        (Nothing, Just (Failure t)) -> withAttr failureAttr $ txtWrap $ "✘ " <> t
        (Nothing, Nothing) -> txt " "
    capital t = case T.uncons t of
        Just (c, rest) -> T.cons (toUpper c) rest
        Nothing -> t

{- | Key hints on one line. A hint naming a key is a button: a
click on it presses the key.
-}
hints :: [(Text, Text)] -> Widget Name
hints = hBox . zipWith hint [0 :: Int ..]
  where
    hint i (k, d) =
        hBox
            [ txt $ if i == 0 then "" else "   "
            , case keyOf k of
                Just key ->
                    clickable (KeyButton key)
                        $ withAttr buttonAttr
                        $ hBox
                            [ txt " "
                            , withAttr buttonKeyAttr $ txt k
                            , txt $ " " <> d <> " "
                            ]
                Nothing -> hBox [withAttr keyAttr $ txt k, txt $ " " <> d]
            ]

-- | The key a hint names, if it names one.
keyOf :: Text -> Maybe V.Key
keyOf = \case
    "enter" -> Just V.KEnter
    "esc" -> Just V.KEsc
    "⌫" -> Just V.KBS
    "tab" -> Just $ V.KChar '\t'
    "any key" -> Just V.KEsc
    t | [c] <- T.unpack t -> Just $ V.KChar c
    _ -> Nothing

-- | The keys that act in the current state.
keysFor :: State -> [(Text, Text)]
keysFor s
    | Just _ <- stBrowser s =
        [ ("↑ ↓  j k", "move")
        , ("enter", "pick the image, or open the directory")
        , ("backspace", "parent directory")
        , ("esc", "cancel")
        ]
    | Just _ <- stDelete s =
        [ ("digits", "the last four digits of the ICCID")
        , ("enter", "delete if they match, else cancel")
        , ("esc", "cancel")
        ]
    | Just _ <- stNicknameEdit s =
        [ ("enter", "set the nickname; empty clears it")
        , ("backspace", "erase")
        , ("esc", "cancel")
        ]
    | Just _ <- stConfirm s =
        [("y", "enable the profile"), ("any key", "cancel")]
    | otherwise = case stView s of
        ProfilesView ->
            [ ("↑ ↓  j k", "move")
            , ("e  enter", "enable the selected profile")
            , ("m", "set its nickname")
            , ("g", "install a plan from a QR code")
            , ("d", "download with address and code")
            , ("D", "delete it (disabled profiles only)")
            , ("n", "pending notifications")
            , ("r", "read the card again")
            , ("q", "quit")
            ]
        NotificationsView ->
            [ ("↑ ↓  j k", "move")
            , ("s", "send the selected notification")
            , ("a", "send them all")
            , ("p  esc", "back to the profiles")
            , ("r", "read the card again")
            , ("q", "quit")
            ]
        _ ->
            [ ("tab ↑ ↓", "next field")
            , ("enter", "continue")
            , ("esc", "cancel")
            ]

helpLayer :: State -> Widget Name
helpLayer s
    | not (stHelp s) = emptyWidget
    | otherwise =
        panel "Keys" $
            [ hBox
                [ hLimit 16 $ padRight Max $ withAttr keyAttr $ txt k
                , txt d
                ]
            | (k, d) <- keysFor s
            ]
                <> [ txt " "
                   , withAttr dimAttr $ txt "ctrl-c quits from anywhere."
                   , txt " "
                   , hCenter $ hints [("any key", "close")]
                   ]

-- Layers -----------------------------------------------------------

browserLayer :: State -> Widget Name
browserLayer s = case stBrowser s of
    Nothing -> emptyWidget
    Just Browser{..} ->
        let window = 14
            top = max 0 $ min (length brItems - window) (brCursor - window `div` 2)
            shown = take window $ drop top $ zip [0 :: Int ..] brItems
        in  panel "Pick the QR image" $
                [ withAttr dimAttr $ txt $ T.pack $ ellipsisLeft 58 brCwd
                , txt " "
                ]
                    <> ( if null brItems
                            then [withAttr dimAttr $ txt "(empty)"]
                            else
                                [withAttr dimAttr $ txt "  ↑ more" | top > 0]
                                    <> [entry (i == brCursor) i e | (i, e) <- shown]
                                    <> [ withAttr dimAttr $ txt "  ↓ more"
                                       | top + window < length brItems
                                       ]
                       )
                    <> [ txt " "
                       , hints [("enter", "pick"), ("⌫", "up"), ("esc", "cancel")]
                       ]
  where
    entry selected i (isDir, name) =
        clickable (PickerEntry i)
            $ withAttr (attrFor selected i isDir name)
            $ padRight Max
            $ txt
            $ (if selected then "› " else "  ")
                <> (if isDir then name <> "/" else name)
    attrFor selected i isDir name
        | selected = selectedAttr
        | isDir = dirAttr
        | isImage name = rowAttr False i
        | otherwise = dimAttr
    isImage name =
        map toLower (takeExtension $ T.unpack name)
            `elem` [".png", ".jpg", ".jpeg", ".gif", ".bmp", ".webp"]
    ellipsisLeft n p
        | length p <= n = p
        | otherwise = "…" <> reverse (take (n - 1) $ reverse p)

confirmLayer :: State -> Widget Name
confirmLayer s = case stConfirm s of
    Nothing -> emptyWidget
    Just p ->
        panel
            "Enable profile"
            [ hCenter $ withAttr titleAttr $ txt $ profileLabel p
            , hCenter $ withAttr dimAttr $ txt $ profileIccid p
            , txt " "
            , txtWrap $ case enabledProfile of
                Just e ->
                    profileLabel e
                        <> " will be disabled. Switching works offline."
                Nothing -> "Switching works offline."
            , txt " "
            , hCenter $ hints [("y", "enable"), ("any key", "cancel")]
            ]
  where
    enabledProfile =
        find ((== Enabled) . profileState) . snapProfiles =<< snapshotOf s

nicknameLayer :: State -> Widget Name
nicknameLayer s = case stNicknameEdit s of
    Nothing -> emptyWidget
    Just (p, t) ->
        panel
            "Nickname"
            [ hCenter $ withAttr titleAttr $ txt $ profileLabel p
            , hCenter $ withAttr dimAttr $ txt $ profileIccid p
            , txt " "
            , hCenter $ input True 40 t
            , txt " "
            , hCenter $
                hints [("enter", "set"), ("empty", "clears"), ("esc", "cancel")]
            ]

deleteLayer :: State -> Widget Name
deleteLayer s = case stDelete s of
    Nothing -> emptyWidget
    Just (p, t) ->
        panelWith
            True
            "Delete profile"
            [ hCenter $ withAttr titleAttr $ txt $ profileLabel p
            , hCenter $ withAttr dimAttr $ txt $ profileIccid p
            , txt " "
            , withAttr dangerAttr $
                txtWrap
                    "Deleting is permanent. Some providers' QR codes \
                    \install only once; check before deleting."
            , txt " "
            , txtWrap $
                "Type the last "
                    <> T.pack (show $ T.length $ deleteCheck p)
                    <> " digits of the ICCID to confirm."
            , txt " "
            , hCenter $ input True 10 t
            , txt " "
            , hCenter $ hints [("enter", "delete if they match"), ("esc", "cancel")]
            ]

snapshotOf :: State -> Maybe Snapshot
snapshotOf s = case stCard s of
    Just (Right snap) -> Just snap
    _ -> Nothing

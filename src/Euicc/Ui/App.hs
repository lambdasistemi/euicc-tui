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
    , customMain
    , emptyWidget
    , fg
    , get
    , hBox
    , hLimit
    , halt
    , neverShowCursor
    , on
    , padBottom
    , padLeftRight
    , padRight
    , put
    , str
    , txt
    , txtWrap
    , vBox
    , withAttr
    )
import Brick.BChan (newBChan, writeBChan)
import Brick.Widgets.Border (borderWithLabel, hBorder)
import Brick.Widgets.Center (centerLayer, hCenter)
import Control.Concurrent (forkIO)
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Data.Foldable (traverse_)
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
    , handleKey
    , smdpDisplay
    , start
    )
import Graphics.Vty qualified as V
import Graphics.Vty.CrossPlatform (mkVty)

-- | A job finished on the worker thread.
newtype AppEvent = JobDone JobResult

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

app :: (Job -> IO ()) -> App State AppEvent ()
app launch =
    App
        { appDraw = draw
        , appChooseCursor = neverShowCursor
        , appHandleEvent = handleEvent launch
        , appStartEvent = pure ()
        , appAttrMap = const attributes
        }

handleEvent
    :: (Job -> IO ()) -> BrickEvent () AppEvent -> EventM () State ()
handleEvent launch = \case
    VtyEvent (V.EvKey key mods) -> do
        s <- get
        case handleKey key mods s of
            Halt -> halt
            Continue s' job -> do
                put s'
                liftIO $ traverse_ launch job
    AppEvent (JobDone r) -> do
        s <- get
        let (s', job) = finishJob r s
        put s'
        liftIO $ traverse_ launch job
    _ -> pure ()

-- Attributes -------------------------------------------------------

selectedAttr, enabledAttr, failureAttr, infoAttr, busyAttr :: AttrName
selectedAttr = attrName "selected"
enabledAttr = attrName "enabled"
failureAttr = attrName "failure"
infoAttr = attrName "info"
busyAttr = attrName "busy"

attributes :: AttrMap
attributes =
    attrMap
        V.defAttr
        [ (selectedAttr, V.black `on` V.cyan)
        , (enabledAttr, fg V.green)
        , (failureAttr, fg V.red)
        , (infoAttr, fg V.cyan)
        , (busyAttr, fg V.yellow)
        ]

-- Drawing ----------------------------------------------------------

draw :: State -> [Widget ()]
draw s = [browserLayer s, deleteLayer s, confirmLayer s, mainLayer s]

mainLayer :: State -> Widget ()
mainLayer s =
    vBox
        [ header s
        , hBorder
        , padBottom Max $ body s
        , hBorder
        , statusLine s
        , helpLine s
        ]

header :: State -> Widget ()
header s = padLeftRight 1 $ hBox [txt "euicc-tui   ", details]
  where
    details = case stCard s of
        Just (Right snap) ->
            let ChipInfo{..} = snapChip snap
            in  txt $
                    "EID "
                        <> chipEid
                        <> "   free "
                        <> maybe "unknown" formatBytes chipFreeMemory
        _ -> txt "no card read"

formatBytes :: Integer -> Text
formatBytes n = T.pack (show $ n `div` 1024) <> " KiB"

body :: State -> Widget ()
body s = case stCard s of
    Nothing -> padLeftRight 1 $ txt "Reading the card..."
    Just (Left f) ->
        padLeftRight 1 $
            vBox
                [ withAttr failureAttr $ txt "The card could not be read."
                , txt " "
                , txtWrap $ describeFailure f
                , txt " "
                , txt "Press r to try again, q to quit."
                ]
    Just (Right snap) -> case stView s of
        ProfilesView -> profilesTable s $ snapProfiles snap
        NotificationsView ->
            notificationsTable s $ snapNotifications snap
        DownloadView -> downloadForm $ stForm s
        WizardView -> wizardBody s

row :: [(Int, Text)] -> Widget ()
row = hBox . map cell
  where
    cell (w, t) =
        hLimit w $ padRight Max $ txt $ fill $ T.take (w - 1) t
    fill t = if T.null t then " " else t

profilesTable :: State -> [Profile] -> Widget ()
profilesTable s = \case
    [] -> padLeftRight 1 $ txt "No profiles on this card."
    ps ->
        padLeftRight 1
            $ vBox
            $ row
                [ (3, "")
                , (28, "Name")
                , (22, "Provider")
                , (22, "ICCID")
                , (10, "State")
                ]
                : zipWith profileRow [0 ..] ps
  where
    profileRow :: Int -> Profile -> Widget ()
    profileRow i p =
        highlight (i == stProfileCursor s)
            $ stateAttr p
            $ row
                [ (3, if profileState p == Enabled then "*" else "")
                , (28, profileLabel p)
                , (22, fromMaybe "" $ profileProvider p)
                , (22, profileIccid p)
                , (10, stateText $ profileState p)
                ]
    stateAttr p
        | profileState p == Enabled = withAttr enabledAttr
        | otherwise = id
    stateText = \case
        Enabled -> "enabled"
        Disabled -> "disabled"
        OtherState t -> t

notificationsTable :: State -> [Notification] -> Widget ()
notificationsTable s = \case
    [] -> padLeftRight 1 $ txt "No pending notifications."
    ns ->
        padLeftRight 1
            $ vBox
            $ row
                [ (6, "Seq")
                , (12, "Operation")
                , (22, "ICCID")
                , (40, "Server")
                ]
                : zipWith notificationRow [0 ..] ns
  where
    notificationRow :: Int -> Notification -> Widget ()
    notificationRow i Notification{..} =
        highlight (i == stNotificationCursor s) $
            row
                [ (6, T.pack $ show notificationSeq)
                , (12, notificationOperation)
                , (22, fromMaybe "" notificationIccid)
                , (40, fromMaybe "" notificationAddress)
                ]

wizardBody :: State -> Widget ()
wizardBody s = case wzPhase w of
    WzSource ->
        vBox
            [ txt "New plan — where does it come from?"
            , txt " "
            , field QrField "QR image       " $ qrDisplay form
            , field SmdpField "SM-DP+ address " $ smdpDisplay form
            , field CodeField "Activation code" $ codeDisplay form
            , txt " "
            , txt "Enter on an empty QR field browses for the image,"
            , txt "or tab to the other fields and type the code."
            , txt "The activation code is never shown."
            ]
    WzReady
        | Just (Download _ _) <- stBusy s ->
            vBox
                [ txt "Installing the plan on the card..."
                , txt " "
                , txt "This takes a few seconds; the list follows."
                ]
        | otherwise ->
            vBox
                [ txt "QR code read."
                , txt " "
                , txt $ "SM-DP+ address   " <> smdpDisplay form
                , txt $ "Activation code  " <> codeDisplay form
                , txt " "
                , txt "enter installs the plan, esc cancels"
                ]
    WzConfirm ->
        vBox
            [ txt "This activation code asks for a confirmation code."
            , txt "The provider sent it separately; it is never shown."
            , txt " "
            , hBox
                [ txt "Confirmation code  "
                , highlight True
                    $ hLimit 50
                    $ padRight Max
                    $ txt
                    $ confirmDisplay w
                ]
            , txt " "
            , txt "enter confirms, esc cancels the install"
            ]
  where
    w = fromMaybe (Wizard WzSource [] "") $ stWizard s
    form = stForm s
    field f label value =
        hBox
            [ txt $ if formFocus form == f then "> " else "  "
            , txt $ label <> "  "
            , highlight (formFocus form == f)
                $ hLimit 50
                $ padRight Max
                $ txt value
            ]

downloadForm :: Form -> Widget ()
downloadForm form =
    padLeftRight 1 $
        vBox
            [ txt "Download a profile"
            , txt " "
            , field SmdpField "SM-DP+ address " $ smdpDisplay form
            , field CodeField "Activation code" $ codeDisplay form
            , txt " "
            , txt "A full LPA:1$... string may be pasted into either field."
            , txt "The activation code is never shown."
            ]
  where
    field f label value =
        hBox
            [ txt $ if formFocus form == f then "> " else "  "
            , txt $ label <> "  "
            , highlight (formFocus form == f)
                $ hLimit 50
                $ padRight Max
                $ txt value
            ]

-- | What the QR field shows: the picked path, or how to pick one.
qrDisplay :: Form -> Text
qrDisplay Form{formQr}
    | T.null formQr = "(enter to browse)"
    | otherwise = formQr

highlight :: Bool -> Widget () -> Widget ()
highlight True = withAttr selectedAttr
highlight False = id

statusLine :: State -> Widget ()
statusLine s = padLeftRight 1 $ case (stBusy s, stStatus s) of
    (Just job, _) ->
        withAttr busyAttr $ txt $ "Working: " <> jobLabel job <> " ..."
    (Nothing, Just (Info t)) -> withAttr infoAttr $ txtWrap t
    (Nothing, Just (Failure t)) -> withAttr failureAttr $ txtWrap t
    (Nothing, Nothing) -> txt " "

helpLine :: State -> Widget ()
helpLine s = padLeftRight 1 $ str $ case stView s of
    ProfilesView ->
        "up/down select  e enable  m nickname  n notifications  \
        \d download  g guided install  D delete  r refresh  q quit"
    NotificationsView ->
        "up/down select  s send selected  a send all  p profiles  \
        \r refresh  q quit"
    DownloadView ->
        "tab switch field  enter download  esc cancel"
    WizardView -> case wzPhase $ fromMaybe (Wizard WzSource [] "") $ stWizard s of
        WzSource -> "tab switch field  enter continue  esc cancel install"
        WzReady -> "enter install  esc cancel"
        WzConfirm -> "type the code  enter confirm  esc cancel install"

browserLayer :: State -> Widget ()
browserLayer s = case stBrowser s of
    Nothing -> emptyWidget
    Just br@Browser{..} ->
        centerLayer
            $ borderWithLabel (txt $ T.pack $ " Pick the QR image — " <> brCwd)
            $ padLeftRight 2
            $ vBox
            $ [txt " "]
                <> [ rowOf br i entry
                   | (i, entry) <- zip [0 :: Int ..] brItems
                   ]
                <> [ txt " "
                   , txt
                        "enter pick or enter a directory, \
                        \backspace up, esc cancel"
                   , txt " "
                   ]
  where
    rowOf Browser{brCursor = cursor} i (isDir, name) =
        highlight (i == cursor)
            $ txt
            $ (if i == cursor then "> " else "  ")
                <> (if isDir then name <> "/" else name)

confirmLayer :: State -> Widget ()
confirmLayer s = case stConfirm s of
    Nothing -> nicknameLayer s
    Just p ->
        centerLayer
            $ borderWithLabel (txt " Enable profile ")
            $ padLeftRight 2
            $ vBox
                [ txt " "
                , hCenter $ txt $ profileLabel p
                , hCenter $ txt $ profileIccid p
                , txt " "
                , txt "The currently enabled profile will be disabled."
                , hCenter $ txt "Enable it? (y/n)"
                , txt " "
                ]

nicknameLayer :: State -> Widget ()
nicknameLayer s = case stNicknameEdit s of
    Nothing -> emptyWidget
    Just (p, t) ->
        centerLayer
            $ borderWithLabel (txt " Nickname ")
            $ padLeftRight 2
            $ vBox
                [ txt " "
                , hCenter $ txt $ profileLabel p
                , hCenter $ txt $ profileIccid p
                , txt " "
                , hCenter
                    $ hLimit 40
                    $ padRight Max
                    $ txt
                    $ if T.null t then " " else t
                , txt " "
                , hCenter $ txt "enter sets, esc cancels, empty clears the field"
                , txt " "
                ]

deleteLayer :: State -> Widget ()
deleteLayer s = case stDelete s of
    Nothing -> emptyWidget
    Just (p, t) ->
        centerLayer
            $ borderWithLabel (withAttr failureAttr $ txt " Delete profile ")
            $ padLeftRight 2
            $ vBox
                [ txt " "
                , hCenter $ txt $ profileLabel p
                , hCenter $ txt $ profileIccid p
                , txt " "
                , withAttr failureAttr $
                    txt "Deleting is permanent. The plan is gone from the card"
                , withAttr failureAttr $
                    txt "and its QR code usually cannot install it again."
                , txt " "
                , hCenter
                    $ txt
                    $ "Type the last "
                        <> T.pack (show $ T.length $ deleteCheck p)
                        <> " digits of the ICCID to delete it:"
                , hCenter
                    $ hLimit 10
                    $ padRight Max
                    $ txt
                    $ if T.null t then " " else t
                , txt " "
                , hCenter $ txt "enter deletes if they match, esc cancels"
                , txt " "
                ]

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use head" #-}
{-# HLINT ignore "Use void" #-}

import           Data.Monoid
import           GHC.IO.Exception
import           System.Exit
import           System.Process
import           Text.Show

import           XMonad
import qualified XMonad.Actions.FlexibleResize  as Flex
import           XMonad.Actions.RepeatAction
import           XMonad.Actions.RotSlaves
import           XMonad.Actions.SpawnOn
import           XMonad.Actions.Volume
import           XMonad.Actions.WithAll

import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Hooks.ManageDocks

import           XMonad.Layout.Dwindle
import           XMonad.Layout.NoBorders
import           XMonad.Layout.Renamed
import           XMonad.Layout.ResizableTile
import           XMonad.Layout.Spacing
import           XMonad.Layout.ThreeColumns
import           XMonad.Layout.ToggleLayouts
import           XMonad.Layout.WindowNavigation

import           XMonad.Util.EZConfig
import           XMonad.Util.Run
import           XMonad.Util.SpawnOnce
import           XMonad.Util.Spotify

import           XMonad.Prompt

import qualified Data.Binary                    as GHC
import qualified Data.Binary                    as GHC.Word
-- import qualified Data.Map                       as M
-- import           Data.Maybe
import qualified Foreign.C
import qualified XMonad.StackSet                as W

import           Colors.Dracula

myFont :: String -> GHC.Word.Word32 -> String
myFont weight size = "xft:FiraCode Nerd Font:weight=" ++ weight ++ ":pixelsize=" ++ show size

myTerminal :: String
myTerminal = "alacritty msg create-window || alacritty"

myBrowser :: String
myBrowser = "firefox"

myMusicPlayer :: String
myMusicPlayer = "spotify"

myEditor :: String
myEditor = "nvim"

-- Whether focus follows the mouse pointer.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = False

-- Whether clicking on a window to focus also passes the click to the window
myClickJustFocuses :: Bool
myClickJustFocuses = True

-- Width of the window border in pixels.
myBorderWidth :: GHC.Word.Word32
myBorderWidth = 1

-- modMask lets you specify which modkey you want to use. The default
-- is mod1Mask ("left alt").  You may also consider using mod3Mask
-- ("right alt"), which does not conflict with emacs keybindings. The
-- "windows key" is usually mod4Mask.
myModMask :: Foreign.C.CUInt
myModMask = mod4Mask

-- The default number of workspaces (virtual screens) and their names.
-- By default we use numeric strings, but any string may be used as a
-- workspace name. The number of workspaces is determined by the length
-- of this list.
--
-- A tagging example:
--
-- > workspaces = ["web", "irc", "code" ] ++ map show [4..9]
myWorkspaces :: [String]
myWorkspaces = ["1", "2", "3", "4", "5", "6", "7", "8", "9"]

-- Border colors for unfocused windows
myNormalBorderColor :: String
myNormalBorderColor = "#000000"

-- Border colors for focused window
myFocusedBorderColor:: String
myFocusedBorderColor = "#008b8b"

recompileAndRestartXMonad :: String
recompileAndRestartXMonad = "notify-send 'Recompiling XMonad' -t 1000 & killall xmobar & xmonad --recompile && xmonad --restart && notify-send 'Recompilation successfully' -t 1000"

myXPConfig = def {
      font         = myFont "regular" 16
    , bgColor      = colorBack
    , fgColor      = colorFore
    , bgHLight     = color06
    , fgHLight     = color01
    , position     = Top
    , height       = 24
    , autoComplete = Just 100000
    }

notify :: Int -> String -> X ()
notify timeout message = spawn $ "notify-send -t " ++ show (timeout * 1000) ++ " " ++ "\"" ++ message ++ "\""

toggleBorders :: X ()
toggleBorders = do
    sendMessage (ModifyScreenBorderEnabled not)
    sendMessage (ModifyWindowBorderEnabled not)
    sendMessage ToggleStruts

-- Maintain only one alacritty process
myRunInTerm :: String -> X ()
myRunInTerm cmd = spawn $ "alacritty msg create-window -e " ++ cmd ++ " || alacritty -e " ++ cmd

edit :: String -> X ()
edit filePath = myRunInTerm $ myEditor ++ " " ++ filePath

fuzzyEdit :: String -> X ()
fuzzyEdit path = myRunInTerm $ "fuzzy.sh edit " ++ path

notifyAndSpawn :: String -> X ()
notifyAndSpawn command = do
    notify 1 $ "Starting " ++ command
    spawnHere command

notifySetVolume volume = do
    setVolume volume
    v <- getVolume

    -- The volume is extremelly close to the actual volume that was set, so it is not a problem to round it
    notify 1 $ "Volume: " ++ show (round v)

viewWS :: Int -> X ()
viewWS i = windows $ W.greedyView $ myWorkspaces !! i

moveToWS :: Int -> X ()
moveToWS i = windows $ W.shift $ myWorkspaces !! i

moveAndViewWS :: Int -> X ()
moveAndViewWS i = moveToWS i >> viewWS i

myKeys :: [(String, X ())]
myKeys = [
    -- launch a terminal
      ("M-<Return>", spawn myTerminal)

    , ("M-m", myRunInTerm "fuzzy.sh manpage")

    -- Make the windows fullscreen
    , ("M-f", sendMessage ToggleLayout)

    -- Aplications to open
    , ("M-o b", notifyAndSpawn myBrowser)
    , ("M-o m", notifyAndSpawn myMusicPlayer)
    , ("M-o d", notifyAndSpawn "discord")

    -- Terminal commands
    , ("M-o h", myRunInTerm "htop")
    , ("M-o c", myRunInTerm "fuzzy.sh cheatsheet")
    , ("M-o l", spawn "fuzzy.sh urls")
    , ("M-o s", spawn "xfce4-screenshooter")

    -- Bluetooth devices that I use
    , ("M-b c p", spawn "bluetooth.sh connect 10 Philips-SHB3175 A4:77:58:79:9E:2F")
    , ("M-b d p", spawn "bluetooth.sh disconnect Philips-SHB3175 A4:77:58:79:9E:2F")
    , ("M-b c r", spawn "bluetooth.sh connect 10 Redmi-Airdots-S 1C:52:16:87:7B:D6")
    , ("M-b d r", spawn "bluetooth.sh disconnect Redmi-Airdots-S 1C:52:16:87:7B:D6")
    , ("M-b c b", spawn "bluetooth.sh connect 10 BT-SPEAKER 16:48:75:47:EF:3D")
    , ("M-b d b", spawn "bluetooth.sh disconnect BT-SPEAKER 16:48:75:47:EF:3D")
    , ("M-b c e", spawn "bluetooth.sh connect 10 887 FC:58:FA:73:76:2A")
    , ("M-b d e", spawn "bluetooth.sh disconnect 887 FC:58:FA:73:76:2A")

    -- Common files thay I edit
    , ("M-e w", edit "~/.xmonad/xmonad.hs")
    , ("M-e t", edit "~/.config/tmux/tmux.conf")

    -- Fuzzy finder for specific paths
    , ("M-e h", fuzzyEdit "~/")
    , ("M-e s", fuzzyEdit "~/school/")
    , ("M-e f", fuzzyEdit "~/.config/fish/")
    , ("M-e n", fuzzyEdit "~/.config/nvim/")
    , ("M-e c", fuzzyEdit "~/.config/ ~/.scripts/ ~/.xmonad/ ~/.local/bin/")
    , ("M-e p", fuzzyEdit "~/.local/share/nvim/site/pack/packer/start/")

    -- Operations with the master window
    , ("M-S-m", windows W.swapMaster) -- Swap the focused window and the master window
    , ("M-S-,", sendMessage $ IncMasterN 1) -- Increment the number of windows in the master area
    , ("M-S-.", sendMessage $ IncMasterN $ -1) -- Decrease the number of windows in the master area
    -- , ("M-m",   windows W.focusMaster) -- Move focus to the master window

    -- Operations with windows
    , ("M-j",   windows W.focusDown)
    , ("M-k",   windows W.focusUp)
    , ("M-h",   sendMessage $ Go L)
    , ("M-l",   sendMessage $ Go R)
    , ("M-S-j", windows W.swapDown)
    , ("M-S-k", windows W.swapUp)
    , ("M-S-h", sendMessage $ Swap L)
    , ("M-S-l", sendMessage $ Swap R)
    , ("M-S-t", withFocused $ windows . W.sink) -- Push window back into tiling
    , ("M-C-j", rotSlavesUp)
    , ("M-C-k", rotSlavesDown)
    , ("M-<Space>", sendMessage NextLayout)   -- Rotate through the available layout algorithms

    , ("M-C-h", sendMessage Shrink) -- Shrink the master area
    , ("M-C-l", sendMessage Expand) -- Expand the master area
    , ("M-C-a", sendMessage MirrorShrink) -- Shrink the other windows
    , ("M-C-z", sendMessage MirrorExpand) -- Expand the other windows

    , ("M-S-q",  kill) -- close focused window
    , ("M-C-q",  killAll) -- close all windows
    , ("M-M1-q", killOthers) -- close all windows without focus

    , ("M-p",   spawn "rofi -show run")
    , ("M-q",   spawn recompileAndRestartXMonad)
    , ("M-S-c", io exitSuccess) -- Quit xmonad

    -- TMUX session manager
    , ("M-S-s", myRunInTerm "tms")

    -- Spotify integration
    , ("M-s j", audioPrev)
    , ("M-s k", audioNext)
    , ("M-s p", audioPlayPause)

    -- Switch and move between workspaces
    , ("M-1",   viewWS        0)
    , ("M-2",   viewWS        1)
    , ("M-3",   viewWS        2)
    , ("M-4",   viewWS        3)
    , ("M-5",   viewWS        4)
    , ("M-6",   viewWS        5)
    , ("M-7",   viewWS        6)
    , ("M-8",   viewWS        7)
    , ("M-9",   viewWS        8)
    , ("M-S-1", moveToWS      0)
    , ("M-S-2", moveToWS      1)
    , ("M-S-3", moveToWS      2)
    , ("M-S-4", moveToWS      3)
    , ("M-S-5", moveToWS      4)
    , ("M-S-6", moveToWS      5)
    , ("M-S-7", moveToWS      6)
    , ("M-S-8", moveToWS      7)
    , ("M-S-9", moveToWS      8)
    , ("M-C-1", moveAndViewWS 0)
    , ("M-C-2", moveAndViewWS 1)
    , ("M-C-3", moveAndViewWS 2)
    , ("M-C-4", moveAndViewWS 3)
    , ("M-C-5", moveAndViewWS 4)
    , ("M-C-6", moveAndViewWS 5)
    , ("M-C-7", moveAndViewWS 6)
    , ("M-C-8", moveAndViewWS 7)
    , ("M-C-9", moveAndViewWS 8)

    -- Volume control
    , ("M-<F1>", raiseVolume 5 >>= \x -> notify 1 $ show x)
    , ("M-<F2>", lowerVolume 5 >>= \x -> notify 1 $ show x)
    , ("M-<F3>", toggleMute >> return ())
    , ("M-v 1",  notifySetVolume 10 )
    , ("M-v 2",  notifySetVolume 20 )
    , ("M-v 3",  notifySetVolume 30 )
    , ("M-v 4",  notifySetVolume 40 )
    , ("M-v 5",  notifySetVolume 50 )
    , ("M-v 6",  notifySetVolume 60 )
    , ("M-v 7",  notifySetVolume 70 )
    , ("M-v 8",  notifySetVolume 80 )
    , ("M-v 9",  notifySetVolume 90 )
    , ("M-v 0",  notifySetVolume 100)

    -- Border keybindings
    , ("M-=",   incScreenWindowSpacing  1)
    , ("M--",   decScreenWindowSpacing  1)
    , ("M-r 0", setScreenWindowSpacing  0)
    , ("M-r 1", setScreenWindowSpacing 10)
    , ("M-r 2", setScreenWindowSpacing 20)
    , ("M-r 3", setScreenWindowSpacing 30)
    , ("M-r 4", setScreenWindowSpacing 40)
    , ("M-r 5", setScreenWindowSpacing 50)
    , ("M-r 6", setScreenWindowSpacing 60)
    , ("M-r 7", setScreenWindowSpacing 70)
    , ("M-r 8", setScreenWindowSpacing 80)
    , ("M-r 9", setScreenWindowSpacing 90)
    , ("M-r t", toggleBorders)

    -- Resize focused window with the mouse
    , ("M-<button3>", withFocused Flex.mouseResizeWindow)
    ]

tall =
      renamed [Replace "Tall"]
    -- $ spacingRaw False (Border 5 5 5 5) True (Border 5 5 5 5) True
    $ smartBorders
    $ avoidStruts
    $ windowNavigation
    $ ResizableTall 1 (2/100) (1/2) []

mirrorTall =
      renamed [Replace "Mirror Tall"]
    -- $ spacingRaw False (Border 5 5 5 5) True (Border 5 5 5 5) True
    $ smartBorders
    $ avoidStruts
    $ windowNavigation
    $ Mirror
    $ ResizableTall 2 (2/100) (1/2) []

tallMasterFocus =
      renamed [Replace "Tall Master Focus"]
    -- $ spacingRaw False (Border 5 5 5 5) True (Border 5 5 5 5) True
    $ smartBorders
    $ avoidStruts
    $ windowNavigation
    $ ResizableTall 1 (2/100) (2/3) []

threeColumns =
      renamed [Replace "ThreeCol"]
    -- $ spacingRaw False (Border 5 5 5 5) True (Border 5 5 5 5) True
    $ smartBorders
    $ avoidStruts
    $ windowNavigation
    $ ThreeCol 1 (2/100) (1/2)

dwindle =
      renamed [Replace "Dwindle"]
    -- $ spacingRaw False (Border 5 5 5 5) True (Border 5 5 5 5) True
    $ smartBorders
    $ avoidStruts
    $ windowNavigation
    $ Dwindle R CW 1 1

full =
      renamed [Replace "Full"]
    $ windowNavigation
    $ noBorders Full

myLayout = toggleLayouts full (dwindle ||| mirrorTall ||| tall ||| tallMasterFocus ||| threeColumns ||| full)

------------------------------------------------------------------------
-- Window rules:

-- Execute arbitrary actions and WindowSet manipulations when managing
-- a new window. You can use this to, for example, always float a
-- particular program, or have a client always appear on a particular
-- workspace.
--
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
-- To match on the WM_NAME, you can use 'title' in the same way that
-- 'className' and 'resource' are used below.
--
-- myManageHook = composeAll
--     [ className =? "MPlayer"        --> doFloat
--         , className =? "Gimp"           --> doFloat
--         , resource  =? "desktop_window" --> doIgnore
--         , resource  =? "kdesktop"       --> doIgnore ]

myManageHook = mempty

------------------------------------------------------------------------
-- Event handling

-- * EwmhDesktops users should change this to ewmhDesktopsEventHook
--
-- Defines a custom handler function for X Events. The function should
-- return (All True) if the default handler is to be run afterwards. To
-- combine event hooks use mappend or mconcat from Data.Monoid.
--
-- myEventHook = swallowEventHook (className =? "Alacritty") (return True)
myEventHook = mempty

------------------------------------------------------------------------
-- Status bars and logging

-- Perform an arbitrary action on each internal state change or X event.
myLogHook x = dynamicLogWithPP xmobarPP {
      ppOutput          = hPutStrLn x
    , ppSep             = " | "
    , ppCurrent         = xmobarColor color06 "" . wrap "[" "]"
    , ppVisible         = xmobarColor color06 ""
    , ppHidden          = xmobarColor color04 "" . wrap "(" ")"
    , ppHiddenNoWindows = xmobarColor color05 ""
    , ppUrgent          = xmobarColor color02 "" . wrap "!" "!"
    , ppOrder           = \(ws:l:_:_) -> ["<fc=#1bb21b> <fn=0> \xf120 </fn> </fc> | " ++ ws, l]
    }

------------------------------------------------------------------------
-- Startup hook

-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.

myStartupHook = do
    spawnOnOnce "workspace1" myTerminal
    spawnOnce   "picom &"

------------------------------------------------------------------------
-- Run xmonad with the settings you specify. No need to modify this.

main = do
    xmproc <- spawnPipe "xmobar -x 0 /home/work/.config/xmobar/xmobar.config"
    xmonad $ docks $ ewmh $ def {
         terminal           = myTerminal
       , focusFollowsMouse  = myFocusFollowsMouse
       , clickJustFocuses   = myClickJustFocuses
       , borderWidth        = myBorderWidth
       , modMask            = myModMask
       , workspaces         = myWorkspaces
       , normalBorderColor  = myNormalBorderColor
       , focusedBorderColor = myFocusedBorderColor
       , layoutHook         = myLayout
       , manageHook         = myManageHook
       , handleEventHook    = myEventHook
       , startupHook        = myStartupHook
       , logHook            = myLogHook xmproc
    } `additionalKeysP` rememberActions "M-." myKeys

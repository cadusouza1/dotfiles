-- xmonad example config file.
--
-- A template showing all available configuration hooks,
-- and how to override the defaults in your own xmonad.hs conf file.
--
-- Normally, you'd only override those defaults you care about.
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use head" #-}

import Text.Show
import GHC.IO.Exception
import Data.Monoid
import System.Exit
import System.Process

import XMonad
import XMonad.Actions.Search
import XMonad.Actions.DynamicProjects
import XMonad.Actions.CycleWS
import XMonad.Actions.RotSlaves
import XMonad.Actions.SpawnOn
import XMonad.Actions.Volume

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.EwmhDesktops

import XMonad.Layout.WindowNavigation
import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableTile
import XMonad.Layout.Spacing
import XMonad.Layout.ThreeColumns 
import XMonad.Layout.Dwindle
import XMonad.Layout.WorkspaceDir

import XMonad.Util.EZConfig
import XMonad.Util.Run
import XMonad.Util.SpawnOnce
import XMonad.Util.Loggers
import XMonad.Util.Spotify

import XMonad.Prompt

import Data.Maybe
import qualified Data.Binary           as GHC
import qualified Data.Binary           as GHC.Word
import qualified Data.Map              as M
import qualified XMonad.Actions.Search as S
import qualified XMonad.Actions.Submap as SM
import qualified XMonad.StackSet       as W
import qualified Foreign.C

import Colors.Dracula

fontSize :: GHC.Word.Word32
fontSize = 16

myFont :: String
myFont = "xft:FiraCode Nerd Font:weight=regular:pixelsize=" ++ show fontSize

myTerminal :: String
myTerminal = "alacritty"

myBrowser:: String
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
myClickJustFocuses = False

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
-- myWorkspaces = ["1", "2", "3", "4", "5", "6", "7", "8", "9", "10"]
myWorkspaces = ["code", "web", "music"] ++ map show [4..9]

-- Border colors for unfocused windows
myNormalBorderColor :: String
myNormalBorderColor = "#000000"

-- Border colors for focused window
myFocusedBorderColor:: String
myFocusedBorderColor = "#008b8b"
--myFocusedBorderColor = "#161925"

recompileAndRestartXMonad :: String
recompileAndRestartXMonad = "notify-send 'Recompiling XMonad' -t 1000 & killall xmobar & xmonad --recompile && xmonad --restart && notify-send 'Recompilation successfully' -t 1000"

projects :: [Project]
projects = []

myXPConfig = def {
      font         = myFont
    , bgColor      = colorBack
    , fgColor      = colorFore
    , bgHLight     = color06
    , fgHLight     = color01
    , height       = fontSize + 8
    , autoComplete = Just 100000
    }

notify timeout message = spawn $ "notify-send -t " ++ show (timeout * 1000) ++ " " ++ "\"" ++ message ++ "\""

toggleBorders :: X ()
toggleBorders = do
    sendMessage (ModifyScreenBorderEnabled not)
    sendMessage (ModifyWindowBorderEnabled not)
    sendMessage ToggleStruts

spawnTUI command = spawn $ myTerminal ++ " -e " ++ command

edit :: String -> X ()
edit filePath = spawnTUI $ myEditor ++ " " ++ filePath

fuzzyEdit :: String -> X ()
fuzzyEdit path = spawnTUI $ "~/.scripts/fzf/fuzzy-edit.sh " ++ path

fuzzyUrlOpen :: X ()
fuzzyUrlOpen = spawnTUI "~/.scripts/fzf/urls/fuzzy-url.sh"

notifyAndSpawn :: String -> X ()
notifyAndSpawn command = do
    notify 1 $ "Starting " ++ command
    spawn command

bluetoothConnect :: String -> String -> X ()
bluetoothConnect deviceID deviceName = do
    notify 1 $ "Connecting to " ++ deviceName
    notify 2 $ "$(bluetoothctl connect " ++ deviceID ++ ")"

bluetoothDisconnect :: String -> String -> X ()
bluetoothDisconnect deviceID deviceName = do
    notify 1 $ "Disconnecting from " ++ deviceName
    notify 2 $ "$(bluetoothctl disconnect " ++ deviceID ++ ")"
     
notifyRaiseVolume :: Double -> X ()
notifyRaiseVolume volume = do
    v <- raiseVolume volume
    notify 1 $ show v

notifyLowerVolume :: Double -> X ()
notifyLowerVolume volume = do
    v <- lowerVolume volume
    notify 1 $ show v

myKeys :: [(String, X ())]
myKeys = [
      ("M-<Return>", spawn myTerminal) -- launch a terminal

    , ("M-<Tab>",   rotSlavesDown)
    , ("M-S-<Tab>", rotSlavesUp)

    -- Quickly create and setup projects
    , ("M-/",         shiftToProjectPrompt myXPConfig)
    , ("M-S-<Space>", switchProjectPrompt myXPConfig)
    
    -- Change the default directory in a workspace
    , ("M-x", changeDir myXPConfig)

    -- Aplications to open
    , ("M-o b", notifyAndSpawn myBrowser)
    , ("M-o m", notifyAndSpawn myMusicPlayer) 
    , ("M-o d", notifyAndSpawn "discord")
    , ("M-o h", spawnTUI "htop")
    , ("M-o l", fuzzyUrlOpen)

    -- Bluetooth devices I use
    , ("M-b c p", bluetoothConnect    "A4:77:58:79:9E:2F" "Philips SHB3175")
    , ("M-b d p", bluetoothDisconnect "A4:77:58:79:9E:2F" "Philips SHB3175")
    , ("M-b c r", bluetoothConnect    "1C:52:16:87:7B:D6" "Redmi Airdots S")
    , ("M-b d r", bluetoothDisconnect "1C:52:16:87:7B:D6" "Redmi Airdots S")
    , ("M-b c b", bluetoothConnect    "16:48:75:47:EF:3D" "BT SPEAKER")
    , ("M-b d b", bluetoothDisconnect "16:48:75:47:EF:3D" "BT SPEAKER")
    , ("M-b c e", bluetoothConnect    "FC:58:FA:73:76:2A" "887")
    , ("M-b d e", bluetoothDisconnect "FC:58:FA:73:76:2A" "887")

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

    -- Operations with the master window
    , ("M-S-m", windows W.swapMaster) -- Swap the focused window and the master window
    -- , ("M-m",   windows W.focusMaster) -- Move focus to the master window
    , ("M-,",   sendMessage $ IncMasterN 1) -- Increment the number of windows in the master area
    , ("M-.",   sendMessage $ IncMasterN $ -1) -- Decrease the number of windows in the master area

    -- Operations with windows
    , ("M-j",   windows W.focusDown)
    , ("M-k",   windows W.focusUp)
    , ("M-h",   sendMessage $ Go L)
    , ("M-l",   sendMessage $ Go R)
    , ("M-S-j", windows W.swapDown)
    , ("M-S-k", windows W.swapUp) 
    , ("M-S-h", sendMessage Shrink) -- Shrink the master area
    , ("M-S-l", sendMessage Expand) -- Expand the master area
    , ("M-S-a", sendMessage MirrorShrink) -- Shrink the other windows
    , ("M-S-z", sendMessage MirrorExpand) -- Expand the other windows
    , ("M-S-t", withFocused $ windows . W.sink) -- Push window back into tiling
    , ("M-S-q", kill) -- close focused window
    , ("M-<Space>", sendMessage NextLayout)   -- Rotate through the available layout algorithms

    , ("M-p",   spawn "rofi -show run")
    , ("M-q",   spawn recompileAndRestartXMonad)
    , ("M-S-c", io exitSuccess) -- Quit xmonad

    -- TMUX sessions
    , ("M-S-s", spawnTUI "tms")
    , ("M-s p", spawnTUI "tms polynomial-calculus")
    , ("M-s i", spawnTUI "tms image-duplicate-detector")
    , ("M-s y", spawnTUI "tms yew-app")

    -- Switch and move between workspaces
    , ("M-1",   windows $ W.greedyView $ myWorkspaces !! 0)
    , ("M-2",   windows $ W.greedyView $ myWorkspaces !! 1)
    , ("M-3",   windows $ W.greedyView $ myWorkspaces !! 2)
    , ("M-4",   windows $ W.greedyView $ myWorkspaces !! 3)
    , ("M-5",   windows $ W.greedyView $ myWorkspaces !! 4)
    , ("M-6",   windows $ W.greedyView $ myWorkspaces !! 5)
    , ("M-7",   windows $ W.greedyView $ myWorkspaces !! 6)
    , ("M-8",   windows $ W.greedyView $ myWorkspaces !! 7)
    , ("M-9",   windows $ W.greedyView $ myWorkspaces !! 8)
    , ("M-S-1", windows $ W.shift      $ myWorkspaces !! 0)
    , ("M-S-2", windows $ W.shift      $ myWorkspaces !! 1)
    , ("M-S-3", windows $ W.shift      $ myWorkspaces !! 2)
    , ("M-S-4", windows $ W.shift      $ myWorkspaces !! 3)
    , ("M-S-5", windows $ W.shift      $ myWorkspaces !! 4)
    , ("M-S-6", windows $ W.shift      $ myWorkspaces !! 5)
    , ("M-S-7", windows $ W.shift      $ myWorkspaces !! 6)
    , ("M-S-8", windows $ W.shift      $ myWorkspaces !! 7)
    , ("M-S-9", windows $ W.shift      $ myWorkspaces !! 8)

    -- Workspace operations
    , ("M-w j", prevWS)
    , ("M-w k", nextWS)
    , ("M-w p", shiftToPrev)
    , ("M-w n", shiftToNext)
    , ("M-w z", toggleWS)

    -- Volume control
    , ("M-<F1>", setMute False >> notifyRaiseVolume 5)
    , ("M-<F2>", setMute False >> notifyLowerVolume 5)
    , ("M-<F3>", toggleMute    >> return ())

    -- Spotify integration
    , ("M-m j", audioPrev)
    , ("M-m k", audioNext)
    , ("M-m p", audioPlayPause)
    ]

tall = 
      spacingRaw False (Border 10 10 10 10) True (Border 10 10 10 10) True 
    $ smartBorders
    $ avoidStruts
    $ windowNavigation
    $ ResizableTall 1 (2/100) (1/2) []

tallMasterFocus = 
      spacingRaw False (Border 10 10 10 10) True (Border 10 10 10 10) True 
    $ smartBorders
    $ avoidStruts
    $ windowNavigation
    $ ResizableTall 1 (2/100) (2/3) []

threeColumns = 
      spacingRaw False (Border 10 10 10 10) True (Border 10 10 10 10) True 
    $ smartBorders
    $ avoidStruts
    $ windowNavigation
    $ ThreeCol 1 (2/100) (1/2)

full =
      spacingRaw False (Border 10 10 10 10) True (Border 10 10 10 10) True 
    $ avoidStruts
    $ windowNavigation
    $ noBorders Full

dwindle = 
      spacingRaw False (Border 10 10 10 10) True (Border 10 10 10 10) True 
    $ smartBorders
    $ avoidStruts
    $ windowNavigation
    $ Dwindle R CW 1 1

myLayout = workspaceDir "~" (dwindle ||| tall ||| tallMasterFocus ||| threeColumns ||| full)

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
    spawnOnOnce "workspace2" myBrowser
    spawnOnOnce "workspace3" myMusicPlayer
    spawnOnce "picom --experimental-backend &"

------------------------------------------------------------------------
-- Run xmonad with the settings you specify. No need to modify this.

main = do 
    xmproc <- spawnPipe "xmobar -x 0 /home/work/.config/xmobar/xmobar.config"
    xmonad $ dynamicProjects projects $ docks $ ewmh $ def {
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
    } `additionalKeysP` myKeys 


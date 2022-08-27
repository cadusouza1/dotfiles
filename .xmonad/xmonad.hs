-- xmonad example config file.
--
-- A template showing all available configuration hooks,
-- and how to override the defaults in your own xmonad.hs conf file.
--
-- Normally, you'd only override those defaults you care about.

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

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.InsertPosition

import XMonad.Layout.TwoPane
import XMonad.Layout.Combo
import XMonad.Layout.Simplest
import XMonad.Layout.Grid
import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableTile
import XMonad.Layout.Spacing
import XMonad.Layout.Tabbed
import XMonad.Layout.ThreeColumns 
import XMonad.Layout.SubLayouts
import XMonad.Layout.WindowNavigation
import XMonad.Layout.BoringWindows
import XMonad.Layout.Dwindle
import XMonad.Layout.BinaryColumn
import XMonad.Layout.WorkspaceDir

import XMonad.Util.EZConfig
import XMonad.Util.Run
import XMonad.Util.SpawnOnce
import XMonad.Util.Loggers

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
myBorderWidth = 2

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
myWorkspaces = ["1", "2", "3", "4", "5", "6", "7", "8", "9", "10"]

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
    notify 2 $ "$(pulseaudio --start && bluetoothctl connect " ++ deviceID ++ ")"

bluetoothDisconnect :: String -> String -> X ()
bluetoothDisconnect deviceID deviceName = do
    notify 1 $ "Disconnecting from " ++ deviceName
    notify 2 $ "$(bluetoothctl disconnect " ++ deviceID ++ ")"
     
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

    -- Workspace operations
    , ("M-w j", prevWS)
    , ("M-w k", nextWS)
    , ("M-w p", shiftToPrev)
    , ("M-w n", shiftToNext)
    , ("M-w z", toggleWS)

    -- Aplications to open
    , ("M-o b", notifyAndSpawn myBrowser)
    , ("M-o t", notifyAndSpawn "teams") 
    , ("M-o d", notifyAndSpawn "discord")
    , ("M-o m", notifyAndSpawn "spotify") 
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
    , ("M-e f", edit "~/.config/fish/config.fish") 

    -- Fuzzy finder for specific paths
    , ("M-e h", fuzzyEdit "~/")
    , ("M-e s", fuzzyEdit "~/school/")
    , ("M-e n", fuzzyEdit "~/.config/nvim/") 
    , ("M-e c", fuzzyEdit "~/.config/ ~/.scripts/ ~/.xmonad/") 
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
    , ("M-m",   windows W.focusMaster) -- Move focus to the master window
    , ("M-,",   sendMessage $ IncMasterN 1) -- Increment the number of windows in the master area
    , ("M-.",   sendMessage $ IncMasterN $ -1) -- Decrease the number of windows in the master area

    -- Operations with windows
    , ("M-j",       focusDown)
    , ("M-k",       focusUp)
    , ("M-h",       sendMessage $ Go L)
    , ("M-l",       sendMessage $ Go R)
    , ("M-S-j",     windows W.swapDown)
    , ("M-S-k",     windows W.swapUp) 
    , ("M-S-h",     sendMessage Shrink) -- Shrink the master area
    , ("M-S-l",     sendMessage Expand) -- Expand the master area
    , ("M-S-a",     sendMessage MirrorShrink) -- Shrink the other windows
    , ("M-S-z",     sendMessage MirrorExpand) -- Expand the other windows
    , ("M-S-t",     withFocused $ windows . W.sink) -- Push window back into tiling
    , ("M-S-q",     kill) -- close focused window
    , ("M-<Space>", sendMessage NextLayout)   -- Rotate through the available layout algorithms

    -- Operations with windows in a sublayout
    , ("M-C-h",  sendMessage $ pullGroup L)
    , ("M-C-l",  sendMessage $ pullGroup R)
    , ("M-C-k",  sendMessage $ pullGroup U)
    , ("M-C-j",  sendMessage $ pullGroup D)
    , ("M-C-m",  withFocused $ sendMessage . MergeAll)
    , ("M-C-n",  withFocused $ sendMessage . UnMerge)
    , ("M-M1-j", onGroup W.focusUp')
    , ("M-M1-k", onGroup W.focusDown')

    , ("M-p",   spawn "rofi -show run")
    , ("M-q",   spawn recompileAndRestartXMonad)
    , ("M-S-c", io exitSuccess) -- Quit xmonad

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
    , ("M-0",   windows $ W.greedyView $ myWorkspaces !! 9)
    , ("M-S-1", windows $ W.shift      $ myWorkspaces !! 0)
    , ("M-S-2", windows $ W.shift      $ myWorkspaces !! 1)
    , ("M-S-3", windows $ W.shift      $ myWorkspaces !! 2)
    , ("M-S-4", windows $ W.shift      $ myWorkspaces !! 3)
    , ("M-S-5", windows $ W.shift      $ myWorkspaces !! 4)
    , ("M-S-6", windows $ W.shift      $ myWorkspaces !! 5)
    , ("M-S-7", windows $ W.shift      $ myWorkspaces !! 6)
    , ("M-S-8", windows $ W.shift      $ myWorkspaces !! 7)
    , ("M-S-9", windows $ W.shift      $ myWorkspaces !! 8)
    , ("M-S-0", windows $ W.shift      $ myWorkspaces !! 9)
    ]

-- setting colors for tabs layout and tabs sublayout.
myTabTheme = def { fontName            = myFont
                 , activeColor         = color07
                 , inactiveColor       = colorBack
                 , activeBorderColor   = color07
                 , inactiveBorderColor = colorBack
                 , activeTextColor     = colorBack
                 , inactiveTextColor   = color16
                 }

tall = 
      spacingRaw False (Border 10 10 10 10) True (Border 10 10 10 10) True 
    $ smartBorders
    $ avoidStruts
    $ windowNavigation 
    $ boringWindows 
    $ addTabs shrinkText myTabTheme
    $ subLayout [] Simplest 
    $ ResizableTall 1 (2/100) (1/2) []

tallMasterFocus = 
      spacingRaw False (Border 10 10 10 10) True (Border 10 10 10 10) True 
    $ smartBorders
    $ avoidStruts
    $ windowNavigation 
    $ boringWindows 
    $ addTabs shrinkText myTabTheme
    $ subLayout [] Simplest 
    $ ResizableTall 1 (2/100) (2/3) []

threeColumns = 
      spacingRaw False (Border 10 10 10 10) True (Border 10 10 10 10) True 
    $ smartBorders
    $ avoidStruts
    $ windowNavigation 
    $ boringWindows 
    $ addTabs shrinkText myTabTheme
    $ subLayout [] Simplest 
    $ ThreeCol 1 (2/100) (1/2)

full =
      spacingRaw False (Border 10 10 10 10) True (Border 10 10 10 10) True 
    $ avoidStruts
    $ noBorders Full

dwindle = 
      spacingRaw False (Border 10 10 10 10) True (Border 10 10 10 10) True 
    $ smartBorders
    $ avoidStruts
    $ windowNavigation 
    $ boringWindows 
    $ addTabs shrinkText myTabTheme
    $ subLayout [] Simplest 
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

-- myManageHook = insertPosition Below Newer
myManageHook = mempty

------------------------------------------------------------------------
-- Event handling

-- * EwmhDesktops users should change this to ewmhDesktopsEventHook
--
-- Defines a custom handler function for X Events. The function should
-- return (All True) if the default handler is to be run afterwards. To
-- combine event hooks use mappend or mconcat from Data.Monoid.
--
myEventHook = ewmhDesktopsEventHook

------------------------------------------------------------------------
-- Status bars and logging

-- Perform an arbitrary action on each internal state change or X event.
myLogHook x = dynamicLogWithPP xmobarPP { 
      ppOutput          = hPutStrLn x
    , ppSep             = " | "
    , ppCurrent         = xmobarColor color06 "" . wrap "[" "]"
    , ppVisible         = xmobarColor color06 ""
    , ppHidden          = xmobarColor color04 "" . wrap "*" ""
    , ppHiddenNoWindows = xmobarColor color05 ""
    , ppUrgent          = xmobarColor color02 "" . wrap "!" "!"
    , ppOrder           = \(ws:l:_:_) -> [ws, l]
    }

------------------------------------------------------------------------
-- Startup hook

-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.

myStartupHook = do
    spawnOnce myTerminal
    spawnOnce "picom --experimental-backend &"

------------------------------------------------------------------------
-- Run xmonad with the settings you specify. No need to modify this.

main = do 
    xmproc <- spawnPipe "xmobar -x 0 /home/work/.xmonad/xmobar.hs"
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


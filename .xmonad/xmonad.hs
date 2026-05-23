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
import           XMonad.Actions.SpawnOn
import           XMonad.Actions.Submap
import           XMonad.Actions.SwapWorkspaces
import           XMonad.Actions.Volume
import           XMonad.Actions.WithAll

import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Hooks.ManageDocks

import           XMonad.Layout.BoringWindows
import           XMonad.Layout.Dwindle
import           XMonad.Layout.Minimize
import           XMonad.Layout.NoBorders
import           XMonad.Layout.Renamed
import           XMonad.Layout.ResizableTile
import           XMonad.Layout.Simplest
import           XMonad.Layout.Spacing
import           XMonad.Layout.SubLayouts
import           XMonad.Layout.Tabbed
import           XMonad.Layout.ThreeColumns
import           XMonad.Layout.ToggleLayouts
import           XMonad.Layout.WindowNavigation
import qualified XMonad.Layout.Magnifier as Mag
import           XMonad.Layout.CenteredMaster

import           XMonad.Util.EZConfig
import           XMonad.Util.Run
import           XMonad.Util.SpawnOnce
import XMonad.Util.ExtensibleState (put, get)

import           XMonad.Prompt

import           System.Directory

import qualified Data.Binary                    as GHC
import qualified Data.Binary                    as GHC.Word
import qualified Data.Map                       as M
import           Data.IORef
import qualified Foreign.C
import qualified XMonad.StackSet                as W
import Data.List (isPrefixOf)

import Control.Monad.IO.Class (MonadIO(liftIO))
import GHC.IO (unsafePerformIO)

import           Colors.Dracula

myTerminal :: String
myTerminal = "st"

myBrowser :: String
myBrowser = "firefox"

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

groupNames :: [String]
groupNames = [ "Default", "Games", "Arq Comp", "Probest", "EPP" ]

-- groups :: [[String]]
-- groups = [ ["1:Lithium", "2:Sodium", "3:Potasium", "4:Rubidium"]
--          , ["5:Beryllium", "6:Magnesium", "7:Calcium", "8:Strontium"]
--          , ["9:Iron", "10:Cobalt", "11:Nickel", "12:Copper"] 
--          , ["13:Aluminum", "14:Galium", "15:Indium", "16:Thalium"]
--          , ["17:Boron", "18:Silicon", "19:Germanium", "20:Arsenic"] ]

groups = [ ["1", "2", "3", "4", "5", "6", "7", "8", "9"]
         , ["10", "11", "12", "13", "14", "15", "16", "17", "18"]
         , ["19", "20", "21", "22", "23", "24", "25", "26", "27"] 
         , ["28", "29", "30", "31", "32", "33", "34", "35", "36"]
         , ["37", "38", "39", "40", "41", "42", "43", "44", "45"] ]

{-# NOINLINE currentGroup #-}
currentGroup = unsafePerformIO $ newIORef 0

getCurrentGroup :: IO Int
getCurrentGroup = readIORef currentGroup

setCurrentGroup :: Int -> IO ()
setCurrentGroup = writeIORef currentGroup

currentGroupWorkspaces :: IO [String]
currentGroupWorkspaces = do
    i <- getCurrentGroup
    return $ groups !! i

switchGroup :: Int -> X ()
switchGroup group = do
    liftIO $ setCurrentGroup group
    viewWS 0

moveToGroup :: Int -> X ()
moveToGroup group = do
    windows $ W.shift $ groups !! group !! 0

moveToAndSwitchGroup :: Int -> X ()
moveToAndSwitchGroup group = do
    moveToGroup group
    switchGroup group

viewWS :: Int -> X ()
viewWS i = do
    group <- liftIO getCurrentGroup
    windows $ W.greedyView $ groups !! group !! i

moveToWS :: Int -> X ()
moveToWS i = do
    group <- liftIO getCurrentGroup
    windows $ W.shift $ groups !! group !! i

swapWorkspace :: Int -> X ()
swapWorkspace i = do
    group <- liftIO getCurrentGroup
    windows $ swapWithCurrent $ groups !! group !! i

moveAndViewWS :: Int -> X ()
moveAndViewWS i = moveToWS i >> viewWS i

-- Border colors for unfocused windows
myNormalBorderColor :: String
myNormalBorderColor = color01

-- Border colors for focused window
myFocusedBorderColor:: String
myFocusedBorderColor = color02

recompileAndRestartXMonad :: String
recompileAndRestartXMonad = "notify-send 'Recompiling XMonad' -t 1000 & xmonad --recompile && xmonad --restart && notify-send 'Recompilation successfully' -t 1000"

notify :: Int -> String -> X ()
notify timeout message = spawn $ "notify-send -t " ++ show (timeout * 1000) ++ " " ++ "\"" ++ message ++ "\""

toggleBorders :: X ()
toggleBorders = do
    sendMessage (ModifyScreenBorderEnabled not)
    sendMessage (ModifyWindowBorderEnabled not)
    sendMessage ToggleStruts

myRunInTerm :: String -> X ()
myRunInTerm cmd = spawn $ myTerminal ++ " -e " ++ cmd

edit :: String -> X ()
edit filePath = myRunInTerm $ myEditor ++ " " ++ filePath

fuzzyEdit :: String -> X ()
fuzzyEdit path = myRunInTerm $ "~/.scripts/fuzzy-edit/fzf-edit " ++ path

notifyAndSpawn :: String -> X ()
notifyAndSpawn command = do
    notify 1 $ "Starting " ++ command
    spawnHere command

notifySetVolume volume = do
    setVolume volume
    v <- getVolume

    -- The volume is extremelly close to the actual volume that was set, so it is not a problem to round it
    notify 1 $ "Volume: " ++ show (round v)


bluetoothConnect name address = spawn $ "~/.scripts/bluetooth.sh connect " ++ name ++ " " ++ address
bluetoothDisconnect name address = spawn $ "~/.scripts/bluetooth.sh disconnect " ++ name ++ " " ++ address

myKeys' conf@(XConfig {XMonad.modMask = modm}) = M.fromList
    [ ((modm, xK_Return), myRunInTerm "tmux new-session -A -s 0")
    , ((modm .|. shiftMask, xK_Return), spawn myTerminal)
    , ((modm, xK_m), myRunInTerm "~/.scripts/manpage/gum-man")
    , ((modm, xK_g), spawn "~/.scripts/name-command-menu ~/.scripts/name-command-menus/gpt-chats.txt")
    , ((modm .|. shiftMask, xK_g), myRunInTerm "~/.scripts/ollama/ollama-saved-chats")
    , ((modm, xK_y), spawn "xdotool search --name 'Youtube' key --clearmodifiers space")

    {- Operations with windows -}
    , ((modm, xK_space), sendMessage NextLayout)
    , ((modm, xK_j), focusDown)
    , ((modm, xK_k), focusUp)
    , ((modm .|. shiftMask, xK_j), windows W.swapDown)
    , ((modm .|. shiftMask, xK_k), windows W.swapUp)
    , ((modm .|. shiftMask, xK_t), withFocused $ windows . W.sink) -- Push window back into tiling

   , ((modm .|. controlMask, xK_equal), sendMessage Mag.MagnifyMore)
   , ((modm .|. controlMask, xK_minus), sendMessage Mag.MagnifyLess)
   , ((modm .|. controlMask, xK_t    ), sendMessage Mag.Toggle)

    {- Operations with sublayouts -}
    , ((modm .|. controlMask, xK_h), sendMessage $ pullGroup L)
    , ((modm .|. controlMask, xK_l), sendMessage $ pullGroup R)
    , ((modm .|. controlMask, xK_k), sendMessage $ pullGroup U)
    , ((modm .|. controlMask, xK_j), sendMessage $ pullGroup D)

    , ((modm .|. controlMask, xK_m), withFocused (sendMessage . MergeAll))
    , ((modm .|. controlMask, xK_u), withFocused (sendMessage . UnMerge))

    , ((modm .|. controlMask, xK_period), onGroup W.focusDown')
    , ((modm .|. controlMask, xK_comma), onGroup W.focusUp')

    , ((modm, xK_h), sendMessage Shrink)
    , ((modm, xK_l), sendMessage Expand)

    , ((modm .|. shiftMask, xK_q), kill) -- close focused window
    , ((modm .|. controlMask, xK_q), killAll) -- close all windows
    , ((modm .|. mod1Mask, xK_q), killOthers) -- close all windows without focus

    {- Make the windows fullscreen -}
    , ((modm, xK_f), sendMessage ToggleLayout)
    , ((modm, xK_x), spawn "xrandr --output eDP --auto")

    {- Operations with the master window -}
    , ((modm, xK_comma), sendMessage $ IncMasterN 1)
    , ((modm, xK_period), sendMessage $ IncMasterN $ -1)
    , ((modm .|. shiftMask, xK_m), windows W.swapMaster)

    , ((modm, xK_q), spawn recompileAndRestartXMonad)
    , ((modm .|. shiftMask, xK_p), spawn "rofi -dpi 1 -normal-window -modi \"drun\" -show drun")
    , ((modm .|. shiftMask, xK_c), io exitSuccess) -- Quit xmonad

    , ((modm, xK_o), submap . M.fromList $
        {- Aplications to spawn -}
        [ ((0, xK_b), notifyAndSpawn myBrowser)
        , ((0, xK_d), notifyAndSpawn "discord")
        , ((0, xK_t), notifyAndSpawn "~/Telegram/Telegram")
        , ((0, xK_o), notifyAndSpawn "obs")
        , ((0, xK_i), notifyAndSpawn "~/Obsidian/obsidian")
        , ((0, xK_g), myRunInTerm "magohud steam")
        , ((0, xK_s), spawn "xfce4-screenshooter")
        , ((0, xK_y), spawn "freetube")
        , ((0, xK_m), spawn "spotify")

        {- Terminal Commands -}
        , ((0, xK_h), myRunInTerm "htop")
        , ((0, xK_n), myRunInTerm "nmtui")
        , ((0, xK_c), myRunInTerm "~/.scripts/cheatsheet/cheatman")
        , ((0, xK_a), spawn "~/.scripts/name-command-menu ~/.scripts/name-command-menus/series.txt")
        , ((0, xK_l), spawn "~/.scripts/url-bookmarks/rofi-bookmark-open ~/.scripts/url-bookmarks/urls.txt")
        , ((shiftMask, xK_l), spawn "~/.scripts/url-bookmarks/rofi-bookmark-new-window ~/.scripts/url-bookmarks/urls.txt")
        , ((shiftMask, xK_g), spawn "_JAVA_AWT_WM_NONREPARENTING=1 geogebra")
        ])

    , ((modm, xK_b), submap . M.fromList $
        {- Bluetooth devices that I use -}
        [ ((0, xK_c), submap . M.fromList $
            [ ((0, xK_m), bluetoothConnect "M103" "39:C6:89:AF:DA:45")
            , ((0, xK_e), bluetoothConnect "887" "FC:58:FA:73:76:2A")
            , ((0, xK_b), bluetoothConnect "BT-SPEAKER" "16:48:75:47:EF:3D")
            , ((0, xK_r), bluetoothConnect "Redmi-Airdots-S" "1C:52:16:87:7B:D6")
            , ((0, xK_p), bluetoothConnect "Philips-SHB3175" "A4:77:58:79:9E:2F")
            , ((0, xK_t), bluetoothConnect "950BT" "41:42:3D:C0:7D:07")
            , ((0, xK_f), bluetoothConnect "Fuxi-H3" "00:09:24:25:0C:51")
            ])

        , ((0, xK_d), submap . M.fromList $
            [ ((0, xK_m), bluetoothDisconnect "M103" "39:C6:89:AF:DA:45")
            , ((0, xK_e), bluetoothDisconnect "887" "FC:58:FA:73:76:2A")
            , ((0, xK_b), bluetoothDisconnect "BT-SPEAKER" "16:48:75:47:EF:3D")
            , ((0, xK_r), bluetoothDisconnect "Redmi-Airdots-S" "1C:52:16:87:7B:D6")
            , ((0, xK_p), bluetoothDisconnect "Philips-SHB3175" "A4:77:58:79:9E:2F")
            , ((0, xK_t), bluetoothDisconnect "950BT" "41:42:3D:C0:7D:07")
            , ((0, xK_f), bluetoothDisconnect "Fuxi-H3" "00:09:24:25:0C:51")
            ])

        {- Brightness Control -}
        , ((0, xK_s), submap . M.fromList $
            [ ((0, xK_t), spawn "xset dpms force off")
            , ((0, xK_m), spawn "~/.scripts/bright-set.sh 0")
            , ((0, xK_1), spawn "~/.scripts/bright-set.sh 10")
            , ((0, xK_2), spawn "~/.scripts/bright-set.sh 20")
            , ((0, xK_3), spawn "~/.scripts/bright-set.sh 30")
            , ((0, xK_4), spawn "~/.scripts/bright-set.sh 40")
            , ((0, xK_5), spawn "~/.scripts/bright-set.sh 50")
            , ((0, xK_6), spawn "~/.scripts/bright-set.sh 60")
            , ((0, xK_7), spawn "~/.scripts/bright-set.sh 70")
            , ((0, xK_8), spawn "~/.scripts/bright-set.sh 80")
            , ((0, xK_9), spawn "~/.scripts/bright-set.sh 90")
            , ((0, xK_0), spawn "~/.scripts/bright-set.sh 100")
            ])

        {- Alternative to look at the battery in full screen -}
        , ((0, xK_b), spawn "~/.scripts/battery.sh")
        ])

    , ((modm, xK_p), submap . M.fromList $
        [ ((0, xK_s), myRunInTerm "~/.scripts/pdf-reader/pdfr ~/school/")
        , ((0, xK_p), myRunInTerm "~/.scripts/pdf-reader/pdfr ~/piano/")
        , ((0, xK_h), myRunInTerm "~/.scripts/pdf-reader/pdfr ~/")
        ])

    , ((modm, xK_e), submap . M.fromList $
        {- Common files thay I edit -}
        [ ((0, xK_w), edit "~/.xmonad/xmonad.hs")
        , ((0, xK_t), edit "~/.config/tmux/tmux.conf")
        , ((0, xK_u), edit "~/.scripts/url-bookmarks/urls.txt")
        , ((0, xK_a), edit "~/.config/alacritty/alacritty.toml")
        , ((0, xK_v), myRunInTerm "vim ~/.vimrc")

        {- Fuzzy finder for specific paths -}
        , ((0, xK_h), fuzzyEdit "~/")
        , ((0, xK_f), fuzzyEdit "~/.config/fish/")
        , ((0, xK_n), fuzzyEdit "~/.config/nvim/")
        , ((0, xK_b), fuzzyEdit "~/.config/xmobar")
        , ((0, xK_d), fuzzyEdit "~/dotfiles/")
        , ((0, xK_c), fuzzyEdit "~/.scripts/ ~/.xmonad/ ~/.local/bin/ ~/.config/")
        , ((0, xK_p), fuzzyEdit "~/.local/share/nvim/site/pack/packer/start/")
        , ((0, xK_o), fuzzyEdit "~/Documents/Kdu/")
        , ((0, xK_s), fuzzyEdit "~/.scripts/")
        , ((shiftMask, xK_s), fuzzyEdit "~/school/")
        ])

    , ((modm, xK_s), submap . M.fromList $
        [ {- Games -}
          ((0, xK_c), myRunInTerm "mangohud steam steam://rungameid/1252780")
        , ((0, xK_e), myRunInTerm "mangohud steam steam://rungameid/1066780")
        , ((0, xK_b), myRunInTerm "mangohud steam steam://rungameid/960090")
        , ((0, xK_r), myRunInTerm "mangohud steam steam://rungameid/291550")
        , ((0, xK_f), myRunInTerm "mangohud steam steam://rungameid/427520")
        , ((0, xK_o), myRunInTerm "mangohud steam steam://rungameid/49520")
        , ((0, xK_x), myRunInTerm "mangohud steam steam://rungameid/323470")
        , ((0, xK_m), myRunInTerm "mangohud steam steam://rungameid/1604000")
        , ((0, xK_a), myRunInTerm "mangohud steam steam://rungameid/22380")
        , ((0, xK_t), myRunInTerm "mangohud steam steam://rungameid/306020")
        , ((0, xK_i), myRunInTerm "mangohud java -jar ~/Downloads/TLauncher/TLauncher.v10/TLauncher.jar")

        {- Spotify integration -}
        , ((0, xK_j), spawn "~/.scripts/mpv/mpv-playlist-prev")
        , ((0, xK_k), spawn "~/.scripts/mpv/mpv-playlist-next") 
        , ((0, xK_p), spawn "~/.scripts/mpv/mpv-playpause")

        {- Workspace swapping -}
        , ((0, xK_1), swapWorkspace 0)
        , ((0, xK_2), swapWorkspace 1)
        , ((0, xK_3), swapWorkspace 2)
        , ((0, xK_4), swapWorkspace 3)
        , ((0, xK_5), swapWorkspace 4)
        , ((0, xK_6), swapWorkspace 5)
        , ((0, xK_7), swapWorkspace 6)
        , ((0, xK_8), swapWorkspace 7)
        , ((0, xK_9), swapWorkspace 8)
        , ((0, xK_0), swapWorkspace 9)
        ])

    {- Switch and move between workspaces and groups -}
    , ((modm, xK_w), submap . M.fromList $
        [ ((0, xK_1), switchGroup 0)
        , ((0, xK_2), switchGroup 1)
        , ((0, xK_3), switchGroup 2)
        , ((0, xK_4), switchGroup 3)
        , ((0, xK_5), switchGroup 4)
        ])

    {- Switch and move between workspaces -}
    , ((modm .|. shiftMask, xK_w), submap . M.fromList $
        [ ((0, xK_1), moveToGroup 0)
        , ((0, xK_2), moveToGroup 1)
        , ((0, xK_3), moveToGroup 2)
        , ((0, xK_4), moveToGroup 3)
        , ((0, xK_5), moveToGroup 4)
        ])

    {- Switch and move between workspaces -}
    , ((modm .|. controlMask, xK_w), submap . M.fromList $
        [ ((0, xK_1), moveToAndSwitchGroup 0)
        , ((0, xK_2), moveToAndSwitchGroup 1)
        , ((0, xK_3), moveToAndSwitchGroup 2)
        , ((0, xK_4), moveToAndSwitchGroup 3)
        , ((0, xK_5), moveToAndSwitchGroup 4)
        ])

    , ((modm, xK_1),                 viewWS 0)
    , ((modm, xK_2),                 viewWS 1)
    , ((modm, xK_3),                 viewWS 2)
    , ((modm, xK_4),                 viewWS 3)
    , ((modm, xK_5),                 viewWS 4)
    , ((modm, xK_6),                 viewWS 5)
    , ((modm, xK_7),                 viewWS 6)
    , ((modm, xK_8),                 viewWS 7)
    , ((modm, xK_9),                 viewWS 8)
    , ((modm .|. shiftMask, xK_1),   moveToWS 0)
    , ((modm .|. shiftMask, xK_2),   moveToWS 1)
    , ((modm .|. shiftMask, xK_3),   moveToWS 2)
    , ((modm .|. shiftMask, xK_4),   moveToWS 3)
    , ((modm .|. shiftMask, xK_5),   moveToWS 4)
    , ((modm .|. shiftMask, xK_6),   moveToWS 5)
    , ((modm .|. shiftMask, xK_7),   moveToWS 6)
    , ((modm .|. shiftMask, xK_8),   moveToWS 7)
    , ((modm .|. shiftMask, xK_9),   moveToWS 8)
    , ((modm .|. controlMask, xK_1), moveAndViewWS 0)
    , ((modm .|. controlMask, xK_2), moveAndViewWS 1)
    , ((modm .|. controlMask, xK_3), moveAndViewWS 2)
    , ((modm .|. controlMask, xK_4), moveAndViewWS 3)
    , ((modm .|. controlMask, xK_5), moveAndViewWS 4)
    , ((modm .|. controlMask, xK_6), moveAndViewWS 5)
    , ((modm .|. controlMask, xK_7), moveAndViewWS 6)
    , ((modm .|. controlMask, xK_8), moveAndViewWS 7)
    , ((modm .|. controlMask, xK_9), moveAndViewWS 8)

    , ((modm, xK_t), submap . M.fromList $
        [ ((0, xK_l), myRunInTerm "~/.scripts/tmux/tmux-selector")
        , ((0, xK_p), myRunInTerm "~/.scripts/tmux/tmux-dir-launch ~/pprojects/ 2")
        , ((shiftMask, xK_p), myRunInTerm "~/.scripts/tmux/tmux-dir-launch ~/pdfs/ 2")
        , ((shiftMask, xK_l), myRunInTerm "~/.scripts/tmux/tmux-dir-launch ~/pprojects/leetcode/ 1")
        , ((0, xK_a), myRunInTerm "~/.scripts/tmux/tmux-dir-launch ~/pprojects/aoc/ 4")
        , ((0, xK_h), myRunInTerm "~/.scripts/tmux/tmux-dir-launch ~ 2")
        , ((0, xK_n), myRunInTerm "~/.scripts/tmux/tmux-dir-launch ~/Documents/Kdu/ 3")
        , ((0, xK_c), myRunInTerm "~/.scripts/tmux/tmux-dir-launch ~/.config/ 2")
        , ((0, xK_d), myRunInTerm "~/.scripts/tmux/tmux-dir-launch ~/dotfiles/ 2")
        , ((0, xK_s), myRunInTerm "~/.scripts/tmux/tmux-dir-launch ~/.scripts/ 2")
        , ((0, xK_m), myRunInTerm "~/.scripts/tmux/tmux-dir-launch ~/math/ 2")
        ])

    , ((modm, xK_F1), raiseVolume 5 >>= \x -> notify 1 $ show $ round x)
    , ((modm, xK_F2), lowerVolume 5 >>= \x -> notify 1 $ show $ round x)
    , ((modm, xK_v), submap . M.fromList $
        [ ((0, xK_t), toggleMute >> return ())
        , ((0, xK_1), notifySetVolume 10 )
        , ((0, xK_2), notifySetVolume 20 )
        , ((0, xK_3), notifySetVolume 30 )
        , ((0, xK_4), notifySetVolume 40 )
        , ((0, xK_5), notifySetVolume 50 )
        , ((0, xK_6), notifySetVolume 60 )
        , ((0, xK_7), notifySetVolume 70 )
        , ((0, xK_8), notifySetVolume 80 )
        , ((0, xK_9), notifySetVolume 90 )
        , ((0, xK_0), notifySetVolume 100)
        , ((0, xK_f), submap . M.fromList $
            [ ((0, xK_1), spawn "~/.scripts/volume/volume.sh 10%")
            , ((0, xK_2), spawn "~/.scripts/volume/volume.sh 20%")
            , ((0, xK_3), spawn "~/.scripts/volume/volume.sh 30%")
            , ((0, xK_4), spawn "~/.scripts/volume/volume.sh 40%")
            , ((0, xK_5), spawn "~/.scripts/volume/volume.sh 50%")
            , ((0, xK_6), spawn "~/.scripts/volume/volume.sh 60%")
            , ((0, xK_7), spawn "~/.scripts/volume/volume.sh 70%")
            , ((0, xK_8), spawn "~/.scripts/volume/volume.sh 80%")
            , ((0, xK_9), spawn "~/.scripts/volume/volume.sh 90%")
            , ((0, xK_0), spawn "~/.scripts/volume/volume.sh 100%")
            ])
        ])

    {- Border keybindings -}
    , ((modm, xK_equal), incScreenWindowSpacing 1)
    , ((modm, xK_minus), decScreenWindowSpacing 1)
    , ((modm, xK_r), submap . M.fromList $
        [ ((0, xK_0), setScreenWindowSpacing  0)
        , ((0, xK_1), setScreenWindowSpacing 10)
        , ((0, xK_2), setScreenWindowSpacing 20)
        , ((0, xK_3), setScreenWindowSpacing 30)
        , ((0, xK_4), setScreenWindowSpacing 40)
        , ((0, xK_5), setScreenWindowSpacing 50)
        , ((0, xK_6), setScreenWindowSpacing 60)
        , ((0, xK_7), setScreenWindowSpacing 70)
        , ((0, xK_8), setScreenWindowSpacing 80)
        , ((0, xK_9), setScreenWindowSpacing 90)
        , ((0, xK_t), toggleBorders)
        , ((0, xK_f), spawn "xrandr -s 1920x1080")
        , ((0, xK_h), spawn "xrandr -s 1280x720")
        ])
    ]

myKeys :: [(String, X ())]
myKeys = [
      ("<XF86MonBrightnessUp>", spawn "lux -a 1%")
    , ("<XF86MonBrightnessDown>", spawn "lux -s 1%")

    {- Volume control -}
    , ("<XF86AudioRaiseVolume>", spawn "pamixer --increase 5 && notify-send -t 1000 $(pamixer --get-volume)")
    , ("<XF86AudioLowerVolume>", spawn "pamixer --decrease 5 && notify-send -t 1000 $(pamixer --get-volume)")
    , ("<XF86AudioMute>", spawn "pamixer --toggle-mute")

    {- Resize focused window with the mouse -}
    , ("M-<button3>", withFocused Flex.mouseResizeWindow)
    , ("M-<button1>", withFocused mouseMoveWindow >> windows W.shiftMaster)
    ]

mySpacing = spacingRaw False (Border 20 20 20 20) True (Border 20 20 20 20) True

tall =
      renamed [XMonad.Layout.Renamed.Replace "Tall"]
    $ mySpacing
    $ smartBorders
    $ ResizableTall 1 (2/100) (1/2) []

mirrorTall =
      renamed [XMonad.Layout.Renamed.Replace "Mirror Tall"]
    $ mySpacing
    $ smartBorders
    $ Mirror
    $ ResizableTall 1 (2/100) (1/2) []

-- threeColumns =
--       renamed [XMonad.Layout.Renamed.Replace "ThreeCol"]
--     $ mySpacing
--     $ smartBorders
--     $ ThreeCol 1 (2/100) (1/2)

dwindle =
      renamed [XMonad.Layout.Renamed.Replace "Dwindle"]
    $ mySpacing
    $ smartBorders
    $ Dwindle R CW 1 1

full =
      renamed [XMonad.Layout.Renamed.Replace "Full"]
    $ noBorders Full

semiFull =
      renamed [XMonad.Layout.Renamed.Replace "Full"]
    $ mySpacing
    $ smartBorders
    $ noBorders Full

myLayout = boringWindows 
    $ toggleLayouts full 
    $ avoidStruts 
    $ Mag.magnifierOff 
    $ windowNavigation (
        dwindle
    ||| tall
    ||| mirrorTall
    ||| semiFull
    )

myManageHook = composeAll
    [ className =? "discord" --> doShift (groups !! 1 !! 1)
    , title =? "Steam" <||> title =? "steam" <||> isSteamApp --> doShift (groups !! 1 !! 0)
    , className =? "spotify" --> doShift (groups !! 1 !! 2)
    ]
    where
        isSteamApp :: Query Bool
        isSteamApp = fmap ("steam_app" `isPrefixOf`) className

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
myLogHook xmproc0 = dynamicLogWithPP xmobarPP {
      ppOutput = hPutStrLn xmproc0
    , ppCurrent = xmobarColor color06 "" . wrap "[" "]"
    , ppVisible = xmobarColor color06 ""
    -- , ppHidden = xmobarColor color12 "" . wrap "(" ")"
    , ppHidden = \ws -> if ws `elem` unsafePerformIO currentGroupWorkspaces
                        then xmobarColor color12 "" . wrap "(" ")" $ ws
                        else ""
    , ppHiddenNoWindows = \ws -> if ws `elem` unsafePerformIO currentGroupWorkspaces
                                then xmobarColor color05 "" ws
                                else ""
    -- , ppHiddenNoWindows = xmobarColor color05 ""
    , ppSep = " | "
    , ppOrder = \(ws:l:ex) -> ("<fc=" ++ color05 ++ ">" ++ groupNames !! unsafePerformIO getCurrentGroup ++ "</fc>" ) : [ws,"<fc=" ++ color03 ++ "><fn=2>\xebeb</fn> </fc>" ++ l]
}

------------------------------------------------------------------------
-- Startup hook

-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
myStartupHook =
    spawnOnce "picom &"
    -- spawnOnOnce "workspace1" myTerminal

------------------------------------------------------------------------
-- Run xmonad with the settings you specify. No need to modify this.
main = do
    home <- getHomeDirectory
    xmproc0 <- spawnPipe "xmobar $HOME/.config/xmobar/xmobar.config"
    xmproc1 <- spawnPipe "xmobar $HOME/.config/xmobar/xmobar-1.config"
    xmonad $ docks $ ewmh $ def {
         terminal           = myTerminal
       , focusFollowsMouse  = myFocusFollowsMouse
       , clickJustFocuses   = myClickJustFocuses
       , borderWidth        = myBorderWidth
       , modMask            = myModMask
       , keys               = myKeys'
       , workspaces         = concat groups
       , normalBorderColor  = myNormalBorderColor
       , focusedBorderColor = myFocusedBorderColor
       , layoutHook         = myLayout
       , manageHook         = myManageHook
       , handleEventHook    = myEventHook
       , startupHook        = myStartupHook
       , logHook            = myLogHook xmproc1
    } `additionalKeysP` myKeys

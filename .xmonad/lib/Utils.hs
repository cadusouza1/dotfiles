module Utils where 

import XMonad
import XMonad.Layout.Spacing
import XMonad.Hooks.ManageDocks

notify timeout message = spawn ("notify-send -t " ++ show (timeout * 1000) ++ " " ++ "\"" ++ message ++ "\"")

edit :: String -> String -> String -> X ()
edit t e filePath = spawn $ t ++ " -e " ++ e ++ " " ++ filePath

fuzzyEdit :: String -> String -> X ()
fuzzyEdit t path = spawn $ t ++ " -e ~/.scripts/fzf/fuzzy-edit.sh " ++ path

spawnTUI t command = spawn $ t ++ " -e " ++ command

toggleBorders :: X ()
toggleBorders = do
    sendMessage (ModifyScreenBorderEnabled not)
    sendMessage (ModifyWindowBorderEnabled not)
    sendMessage ToggleStruts

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

openBrowserTab b url = do
    notify 1 $ "Opening " ++ url
    spawn $ b ++ " --new-tab https://" ++ url


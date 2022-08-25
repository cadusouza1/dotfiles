#!/bin/sh

connect() {
    pulseaudio --start
    notify-send "Connecting to $1"
    notify-send "$(bluetoothctl connect $2)"
}

disconnect() {
    notify-send "Disconnecting from $1"
    notify-send "$(bluetoothctl disconnect $2)"
}

if [[ $1 == "connect" ]]; then
    connect $1 $2
elif [[ $1 == "disconnect" ]]; then
    disconnect $1 $2
else
    notify-send "Command not recognized"
fi


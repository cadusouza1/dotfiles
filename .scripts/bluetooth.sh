#!/bin/bash

connect() {
    notify-send -t 1000 "Connecting to $1"
    bluetoothctl connect $2
}

disconnect() {
    notify-send -t 1000 "Disconnecting from $1"
    bluetoothctl disconnect $2
}

# Check if the function exists (bash specific)
if declare -f "$1" >/dev/null; then
    # call arguments verbatim
    "$@"
else
    # Show a helpful error
    echo "'$1' is not a known function name" >&2
    exit 1
fi

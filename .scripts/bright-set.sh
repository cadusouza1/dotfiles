#!/usr/bin/env sh

host=$(hostnamectl hostname)
if [[ "$host" = "mmx" ]]; then
    ddcutil setvcp 10 $1
elif [[ "$host" = "ideapad-3-15alc6" ]]; then
    lux -S $1
fi

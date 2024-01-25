#!/bin/bash

stts=$(cat /sys/class/power_supply/BAT0/status)
cap=$(cat /sys/class/power_supply/BAT0/capacity)
notify-send -t 1000 "$stts: $cap%"

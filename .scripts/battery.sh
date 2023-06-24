#!/bin/bash

notify-send -t 1000 "$(cat /sys/class/power_supply/BAT0/status): $(cat /sys/class/power_supply/BAT0/capacity)%"

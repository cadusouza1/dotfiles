#!/bin/bash

sudo brightnessctl s $1
notify-send -t 1000 "Brightness: $1"

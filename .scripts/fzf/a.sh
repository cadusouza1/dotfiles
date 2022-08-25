#!/bin/bash

fdfind -H -t d . $@ | fzf | xargs -r alacritty --working-directory 

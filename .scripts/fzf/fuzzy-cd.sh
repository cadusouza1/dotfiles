#!/bin/bash

fdfind -H -L -t d . $@ | fzf | xargs -r alacritty --working-directory 

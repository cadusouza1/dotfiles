#!/bin/bash

fdfind -H -L -t f . $@ | fzf | xargs -r $EDITOR

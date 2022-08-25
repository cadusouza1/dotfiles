#!/usr/bin/env sh

ps -A | rg teams | awk '{print $1}' | head -n 1 | xargs kill


#!/usr/bin/env sh

amixer -c 2 cset numid=9 $1
amixer -c 2 cset numid=10 $1

#!/usr/bin/env sh

amixer -c 0 cset numid=9 $1
amixer -c 0 cset numid=10 $1

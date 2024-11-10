#!/usr/bin/env bash

rm -v *.wav
ssh $1 "rm -v $2/*"

ls | tail -n $3 | parallel --progress ffmpeg -i {} -ar 16000 -ac 1 {.}.wav
rsync -avz *.wav $1:$2/

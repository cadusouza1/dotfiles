#!/usr/bin/env bash

rm -v *.wav
ssh -v $1 'rm $2/*'

ls | tail -n $3 | parallel --progress ffmpeg -i {} -ar 16000 -ac 1 {.}.wav
rsync -avz *.wav $1:$2/

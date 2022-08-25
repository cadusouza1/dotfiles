#!/bin/bash

programming_projects_dir=~/programming-projects/

cd ~/programming-projects/$(ls -1 $programming_projects_dir | dmenu -l 10)

if [ -d "venv" ] 
then
    . venv/bin/activate
fi

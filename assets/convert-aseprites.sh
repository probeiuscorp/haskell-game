#!/bin/sh
find *.aseprite -exec bash -c 'f="{}"; /home/caleb/repos/aseprite/build/bin/aseprite -b "{}" --save-as "${f%.*}".bmp' \;

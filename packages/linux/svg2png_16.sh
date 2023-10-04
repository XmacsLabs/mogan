#!/bin/bash

echo "Converting $1.svg"

inkscape --batch-process -d 600 -w 16 -h 16 $1.svg -o $1.png
inkscape --batch-process -d 600 -w 32 -h 32 $1.svg -o $1_x2.png
inkscape --batch-process -d 600 -w 64 -h 64 $1.svg -o $1_x4.png


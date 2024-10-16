#!/bin/bash

echo "Converting $1.svg"

inkscape --batch-process -d 600 -w 20 -h 20 $1.svg -o $1.png
inkscape --batch-process -d 600 -w 40 -h 40 $1.svg -o $1_x2.png
inkscape --batch-process -d 600 -w 80 -h 80 $1.svg -o $1_x4.png


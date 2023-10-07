#!/bin/bash

echo "Converting $1.svg"

inkscape --batch-process -d 600 -w 24 -h 24 $1.svg -o $1.png

inkscape --batch-process -d 600 -w 48 -h 48 $1.svg -o $1_x2.png

inkscape --batch-process -d 600 -w 96 -h 96 $1.svg -o $1_x4.png


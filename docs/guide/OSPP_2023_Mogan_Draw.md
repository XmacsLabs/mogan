# Project: Mogan Draw on WASM
+ OSPP 2023 Page: https://summer-ospp.ac.cn/2023/org/prodetail/23b740366?lang=zh&list=pro
+ Forum: http://forum.texmacs.cn/t/project-mogan-draw-on-wasm/1424
+ Tech Tag: Wasm, C++

## Description
Improve the built-in tool for making technical graphics and make it user-friendly enough for most users.

Imagine that you are a teacher in a junior high school teaching Euclid’s Elements, and you are using Mogan Editor/GNU TeXmacs to prepare the slides.

Here is the link to Euclid’s Element (in Greek), you should be able to re-draw the figures in Euclid’s Element via Mogan Draw in an easy way (straight-forward and no need to remember any tricks) and can persuade your colleagues to use Mogan Draw.

http://www.physics.ntua.gr/~mourmouras/euclid/

## Output Requirements
+ User Interface Re-design: make it easy-to-use and straight-forward
+ A report or design doc of the new interface and its comparison between Quiver and Mathcha
+ New features and bug fixes for math students and teachers
  - feature: Draw a circle with its center and radius and more basic commonly used figures
  - feature: Implement various kinds of arrows and decorations
  - bug fix: Fix severe bugs in the current implementation
+ Mogan Draw: the wasm app
  - host it on https://draw.mogan.app with the help of the community
  - make it as smaller as possible
  - make it a standalone app for drawing figures
+ re-draw some figures in Publicity: Euclid’s Element via Mogan Draw and create videos to show the detailed steps to draw the figures and then upload it to Bilibili or YouTube

## Technical Requirements
+ xmake, C++, Scheme
+ Understanding the data model of the Graphics tool
+ Understanding the layout engine
+ Basic Knowledge Computer Graphics

## Project Repository
Choose one of them:
+ https://codeberg.org/XmacsLabs/mogan
+ https://github.com/XmacsLabs/mogan
+ https://gitee.com/XmacsLabs/mogan

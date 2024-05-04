# Mogan STEM Suite v1.2.5 LTS
Mogan STEM Suite v1.2.5 LTS:
+ Mogan Research v1.2.5 LTS

This release is for bug fixes and user experience improvements.

## Notable Changes in v1.2.5.4 (2024/05/04)
+ Graphics mode
  - Fix copy/cut of the whole graphics area when it was selected via mouse
+ User Interface
  - Add `View->Focus mode` as a replacement of `View->Header bars`

## Notable Changes in v1.2.5.3 (2024/04/15)
+ Plugins
  - The Python plugin now supports using virtual environments created by conda
+ Bug Fixes
  - Fixed the issue that using a separate window for structured replacement may cause Mogan to crash

## Notable Changes in v1.2.5.2 (2024/04/08)
+ Bug fixes
  + Fixed the issue where single replacement and replace all functions in the bottom toolbar do not work when clicked with a mouse
  + Fixed the issue where the "File -> Export -> Editable PDF" function does not work when the PDF version is greater than 1.4
  + Fixed the layout engine error caused by executing `ones (0)` in the Octave plugin
  + Fixed the issue where the Portuguese dictionary file is ineffective due to incorrect naming
  + Fixed the issue where styles and images in `$HOME/Documents` cannot be referenced on the macOS platform
  + Set `qt6-qpa-plugins` as an explicit dependency to solve the issue of not being able to start normally after installation on Ubuntu
+ Slide Styles
  + Adjusted the default page type to `16:9`
  + Defaulted slide presentation to fit width
  + When the slides are rendered as book, the document will be automatically adjusted to fit the width upon opening.
+ Plugins
  + Unified the calls to executable files into binary plugins
  + Built-in Python plugin, where the location of the Python interpreter is determined by the Python binary plugin

## Notable changes in v1.2.5.1 (2024/03/14)
+ Shortcuts
  + Fix for the unexpected commit of the ESC key
  + Fix the keyboard insert of backtick (For the SICP Open Course)
+ User Interface
  + Fix messy UI style of `View->Full screen mode`
+ Beamer mode
  + Add the slidemove plugin authored by [@woutersj](https://github.com/woutersj) to adjust the orders of the slides
+ Scheme session
  + Fix encoding of `scheme-eval` (For the SICP Open Course)


## Notable changes in v1.2.5 (2024/02/27)
+ Installers
  + Official deb installer for Ubuntu 20.04
  + Official deb installer for Debian 12 (bookworm)
  + Portable installer for Windows 10/11
+ Beamer mode
  + Fix failure to change the background image when changing the theme on macOS
+ Shortcuts
  + macOS: Command +/- with/without shift to zoom in/out
  + Fix unexpected commit of unrecognized shortcuts
+ User Interface
  + Fix open file via double click with unicode/CJK name on macOS
+ Layout Engine
  + Fix the region invalidation issue when deleting
  + Fix failure when setting the columns number of the specific content to 2 in beamer mode
  + Support numbers in Chinese for the number primitive
+ Performance Improvements
  + Improve the speed of parsing TeXmacs documents
  + Improve the speed of rendering Pine beamer theme
+ PDF Import
  + Add `File->Import->PDF with embedded document...` instead of `File->Open` the "editable" PDF document
+ Version Control
  + Fix the viewer of the history version in Version->History on Windows
+ Image plugins
  + Improve the support for Postscript image format
  + Improve the support for SVG image format
+ Session plugin
  + Fix the encoding of CJK characters before and after scheme-eval
+ Code plugin
  + Re-add the syntax highlighter for Python

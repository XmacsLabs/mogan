# Mogan STEM Suite v1.2.5 LTS
Mogan STEM Suite v1.2.5 LTS:
+ Mogan Research v1.2.5 LTS

This release is for bug fixes and user experience improvements.

## Detailed changes that affect users
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
+ Image plugins
  + Improve the support for Postscript image format
  + Improve the support for SVG image format
+ Git version plugin
  + Fix the viewer of the history version in Version->History on Windows
+ Scheme Session plugin
  + Fix the encoding of CJK characters before and after scheme-eval

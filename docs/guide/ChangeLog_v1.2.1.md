# Mogan STEM Suite v1.2.1
Mogan STEM Suite v1.2.1:
+ Mogan Research v1.2.1

## Known Issues
+ It will crash easily when activating the structural search/replacing window
  + Solution: use v1.2.2 or later version
+ It will crash easily when switching to `Edit->Preference->General->Buffer Management->Documents in separate windows`
  + Solution: use v1.2.2 or later version

## Major Changes
+ Fixed failure of `File->Export->LaTeX` introduced in v1.2.0
+ Fixed crash when clicking `Edit->Keyboard->Keyboard Editor` introduced in v1.2.0
+ Packaging: Create desktop shortcut and start menu shortcut on Windows
+ Packaging: fixed the missing `.desktop` file on Linux
+ Packaging: Support UOS (arm64 and loongarch64)
+ Performance: Improved font finding for truetype fonts to speedup startup for new users
+ UI: Add Chinese translation for all the menu entries (not including submenu), and improved several Chinese translation
+ UI: `File-Open` defaults to the user's home
+ UI: close an maximized Mogan and then launch Mogan, the window is still maximized without any shift on Windows
+ UI: switched to file explorer implemented in Qt to avoid the failure to open files occasionally
+ UI: add 宋体/黑体/仿宋/楷体 to the short font menu for Chinese document
+ Style: invalidate the style cache when a style is changed
+ Editing: Switch to the kde shortcut style for Deepin desktop and KDE desktop on Linux
+ Versioning: Fixed several bugs on Git versioning
+ Stability：Fixed several severe bugs which will freeze or crash Mogan

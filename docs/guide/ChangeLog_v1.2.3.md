# Mogan STEM Suite v1.2.3
Mogan STEM Suite v1.2.3:
+ Mogan Research v1.2.3

## Important Changes
+ Performance improvements
+ Many bug fixes
+ User Interface improvements

## Detailed changes that affect users
+ User Interface
  + Make `Tools->Keyboard->Show key presses` work on Windows and Linux
  + Fix: crashes at opening on macOS arm with external monitor
  + Fix: no priviledge to access `$HOME/Document` on macOS
+ Editor
  + Add `std V`（`Ctrl+Shift+v` or `Command+Shift+v`）to paste verbatim
  + Add the menu entry `Edit->Paste verbatim`
  + Remove `Primary/Seconday/Ternary` in Edit->`Copy/Paste/Cut`
  + macOS: use `Command+left` or `Command+right` to move by word
  + Insert hlink (`[ ] var`) or slink (`[ ] var var`) using markdown style shortcuts
  + Fix input method lost when the cursor is at the start of the line especially for old computers
  + Fix failure to zoom out by `Command+minus` on macOS
+ Font
  + Improve support on the Noto CJK fonts
  + Set default CJK fonts to Noto CJK on all platform if exists
  + Support CESI fonts
+ Plugins
  + Maxima: Fixed the slowness in the editing area
  + Maxima: visit the remote online Maxima manual when click the Help icon on the focus toolbar
  + Spell: enable `Edit->Spell` on Windows and Linux
  + Spell: Fixed the crash on Windows when neither `hunspell` nor `aspell` is installed
  + LaTeX: Fixed the incorrect encoding when copying the several equations to LaTeX snippet

## Changes for Developer
+ Use `xmake-requires.lock` to make the build system more reproducible
+ the installer on macOS arm is now packaged by github action
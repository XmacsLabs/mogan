## Detailed changes that affect users
+ Editor
  + Add `std V`（`Ctrl+Shift+v` or `Command+Shift+v`）to paste verbatim
  + Add the menu entry `Edit->Paste verbatim`
  + Remove `Primary/Seconday/Ternary` in Edit->`Copy/Paste/Cut`
  + Fix input method lost when the cursor is at the start of the line especially for old computers
  + macOS: use `Command+left` or `Command+right` to move by word
  + Insert hlink (`[ ] var`) or slink (`[ ] var var`) using markdown style shortcuts
+ Font
  + Improve support on the Noto CJK fonts
  + Set default CJK fonts to Noto CJK on all platform if exists
  + Support CESI fonts

## Changes for Developer
+ Use `xmake-requires.lock` to make the build system more reproducible
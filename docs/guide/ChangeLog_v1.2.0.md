# Mogan STEM Suite v1.2.0
Since v1.2.0, Mogan Editor has been renamed to Mogan STEM Suite.

Mogan STEM Suite v1.2.0:
+ Mogan Research v1.2.0

Mogan Research is the only product in Mogan STEM Suite for now. Mogan Code and Mogan Beamer will be released later.

## Major Changes
Compared with GNU TeXmacs 2.1.2:
+ OSPP Project `Mogan Draw on wasm`:
  + Improved the user experiences
  + Make it work via wasm in web browsers
+ OSPP Project `Editable PDF`:
  + Export the buffer to PDF and attach the tm doc as the attachment via `File -> Export -> Pdf with embedded document...`
  + `File -> Open` the PDF doc with tm attachment in Mogan
  + Edit and `File -> Save` the latest PDF with latest tm attachment in Mogan
+ New menu entry for the community: Help->Planet
+ Upgrade to Qt 6.5.3
+ Adopt S7 Scheme as the Scheme engine to improve performance
+ Adopt KDE Breeze icons to beautify the UI
+ Fixed many dead shortcuts on Windows and macOS
+ Improved the UI experience: eg. show available shortcuts, add translations
+ Improved the layout engine for Chinese
+ Several improvements on fonts
+ Several improvements for Biliography and Table
+ Adjust the default user preferences for better user experience
+ Adjust the path of TEXMACS_HOME_PATH on Linux/macOS/Windows
+ Fixed several bugs which can crash or freeze Mogan
+ Remove the built-in docs, load the latest GNU TeXmacs online docs
+ Experimental brower-based Mogan Research on wasm

## Know Issues
+ It will crash when clicking `Edit->Keyboard->Edit keyboard shortcuts`
+ For the first installation, it is very slow to open the app because it is loading all the fonts

v1.2.1 will be released before 2024/01/01 to solve the above issues.

## Changes for Developers
+ Use xmake 2.8.5 as the build tool and setup CICD for Ubuntu/macOS/Windows
+ On Windows, use msvc instead mingw to build the app
+ Move basic routines to the lolly project, and depends on liblolly
+ Use tbox as the impl for cross-platform filesystem access and encoding
+ The first step to pluginize the HTML conversion, LaTeX conversion, natural language dictionaries and code syntax highlighting
+ Use the tm docs under devel directory for project management, and setup the convension for commit message
+ Keep project management index for the current version, bug template, feature template under the `Developer` menu

use platform

# Remove xmake local and global cache
rm -rf .xmake
rm -rf build
if $platform:is-windows {
  rm -rf $E:LOCALAPPDATA/.xmake
} else {
  rm -rf ~/.xmake
}

# pin XMAKE_MAIN_REPO
set E:XMAKE_MAIN_REPO = https://gitee.com/XmacsLabs/xmake-repo.git
xmake repo -u --verbose

if $platform:is-windows {
  xmake g --mingw=C:/Qt/Tools/mingw810_64
}

xmake g --proxy_pac=$E:PWD/pac.lua


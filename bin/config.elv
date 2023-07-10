use platform

if $platform:is-windows {
  set paths = [
    ~/scoop/shims/
    C:\Qt\Tools\mingw810_64\bin
  ]
}

rm -rf .xmake
rm -rf build
xmake config --yes -vD --plat=mingw --mode=releasedbg --mingw=C:/Qt/Tools/mingw810_64 --qt=C:\Qt\5.15.2\mingw81_64

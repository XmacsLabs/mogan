# Contributing
## For Users
+ Reporting bugs to [Github in English](https://github.com/XmacsLabs/mogan/issues) or [Gitee in Chinese](https://gitee.com/XmacsLabs/mogan/issues)

## For Developers
Using xmake is encouraged, we will finally deprecate the CMake support!

> WARNING: CMake build is broken because of [PDFWriter and LibAesgm deprecation](https://github.com/XmacsLabs/mogan/pull/574).

For the kernel part, here is how to build and run unit tests:
```
xmake build --group=kernel_tests
xmake run --group=kernel_tests
```

Here are developing guides on Windows/macOS/Linux:
+ Developing on Windows ([English](https://mogan.app/guide/Develop_on_Windows.html)/[中文](https://mogan.app/zh/guide/Develop_on_Windows.html))
+ Developing on macOS ([English](https://mogan.app/guide/Develop_on_macOS.html)/[中文](https://mogan.app/zh/guide/Develop_on_macOS.html))
+ Developing on Linux ([English](https://mogan.app/guide/Develop_on_Linux.html)/[中文](https://mogan.app/zh/guide/Develop_on_Linux.html))

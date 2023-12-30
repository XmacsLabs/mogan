# 墨干和GNU TeXmacs的区别
## 产品定位
墨干是自由的理工套件，包含墨砚、墨码、墨板三大组件。其中墨砚是GNU TeXmacs发行版之一，其定位和GNU TeXmacs一致，都是自由的科技编辑平台。

墨砚作为GNU TeXmacs的发行版，在使用体验上尽可能和GNU TeXmacs保持一致，是由不同团队发行的同一类产品。墨码和墨板则是基于GNU TeXmacs排版引擎和结构化编辑器的全新的产品。

本文着重强调墨砚和GNU TeXmacs这两个同类产品的区别：

## TEXMACS_HOME_PATH
| 操作系统 | 墨砚 | GNU TeXmacs |
|---------|-----|-------------|
| Linux   | `$XDG_DATA_HOME/XmacsLabs` | `$HOME/.TeXmacs` |
|         | `$HOME/.local/share/XmacsLabs` |  |
| macOS | `$HOME/Library/Application Support/XmacsLabs` | `$HOME/.TeXmacs` |
| Windows | `%APPDATA%\XmacsLabs` | `%APPDATA%\TeXmacs`|
| | `C:\Users\用户名\AppData\Roaming\XmacsLabs` | `C:\Users\用户名\AppData\Roaming\TeXmacs` |

> 为什么：不同的路径是为了在系统上可以同时安装墨砚和GNU TeXmacs，另外Linux和macOS的路径更改是为了符合Linux和macOS相关标准。

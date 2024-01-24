# 墨干和GNU TeXmacs的区别
## 产品定位
墨干是自由的理工套件，包含墨砚、墨码、墨板三大组件。其中墨砚是GNU TeXmacs发行版之一，其定位和GNU TeXmacs一致，都是自由的科技编辑平台。

墨砚作为GNU TeXmacs的发行版，在使用体验上尽可能和GNU TeXmacs保持一致，是由不同团队发行的同一类产品。墨码和墨板则是基于GNU TeXmacs排版引擎和结构化编辑器的全新的产品。

本文着重强调墨砚和GNU TeXmacs这两个同类产品的区别：

## 首选项
| 选项 | 墨砚 | GNU TeXmacs|
|--------|-----------------|------------|
| 通用`->`显示询问 | `通过弹出窗口` | `在状态栏` |
| 其他`->`执行文档更新 | `三次` | `一次` |
| 转换`->`PDF`->`展开幻灯片中的可折叠对象 | `开启` | `关闭` |

## 新建文档
### 默认启用`焦点->布局->显示纸面边白`（自V1.2.3起）
为什么：
1. 提供和WPS/MS Word类似的新空白文档体验，让新用户更容易上手
2. `插入->注记->边注`可以立即生效，避免新用户误以为该功能不可用
3. 当缩放比例是100%时，默认页面和导出的PDF以及打印在A4纸上几乎完全一样（实际使用A4纸对比，在左侧和右侧分别有10px的宽度未在屏幕上显示）

### 默认启用`焦点->布局->不显示页码`（自V1.2.3起）
不显示页码，让页面更加干净整洁。

## 快捷键
### 结构化变元轮换（自V1.2.3起）
全平台新增`A-S-up`和`A-S-down`这两个快捷键，用于在结构化变元之间切换。

在Windows和Linux平台仍然保留`C-tab`和`C-S-tab`的旧快捷键。在macOS平台，由于已经将`structured:cmd`从`Option`切换为了`Ctrl`，而且`C-tab`不可用，故而旧快捷键不生效。

> 为什么：因为`C-tab`/`C-S-tab`/`A-S-tab`这三个快捷键在macOS平台不可用，详见[QTBUG-12232](https://bugreports.qt.io/browse/QTBUG-12232)。


## 绘图模式
| 菜单项 | 墨砚 | GNU TeXmacs |
|--------|------------------|-----------|
| 插入`->`图像`->`绘制图形 | 显示网格 | 不显示网格 |
| 插入`->`图像`->`在当前焦点处绘制 | 显示网格 | 不显示网格 |
| 插入`->`网格`->`单位长度 | 2 | 1 |


## TEXMACS_HOME_PATH
| 操作系统 | 墨砚 | GNU TeXmacs |
|---------|-----|-------------|
| Linux   | `$XDG_DATA_HOME/XmacsLabs` | `$HOME/.TeXmacs` |
|         | `$HOME/.local/share/XmacsLabs` |  |
| macOS | `$HOME/Library/Application Support/XmacsLabs` | `$HOME/.TeXmacs` |
| Windows | `%APPDATA%\XmacsLabs` | `%APPDATA%\TeXmacs`|
| | `C:\Users\用户名\AppData\Roaming\XmacsLabs` | `C:\Users\用户名\AppData\Roaming\TeXmacs` |

> 为什么：不同的路径是为了在系统上可以同时安装墨砚和GNU TeXmacs，另外Linux和macOS的路径更改是为了符合Linux和macOS相关标准。

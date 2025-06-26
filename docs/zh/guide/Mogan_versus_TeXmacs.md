# 墨干理工套件和GNU TeXmacs的区别
## 产品定位
墨干理工套件是自由的理工套件，包含墨干、墨码、墨板三大组件。其中墨干（Mogan Research）是GNU TeXmacs发行版之一，其定位和GNU TeXmacs一致，都是自由的科技编辑平台。

墨干作为GNU TeXmacs的发行版，在使用体验上尽可能和GNU TeXmacs保持一致，是由不同团队发行的同一类产品。墨码和墨板则是基于GNU TeXmacs排版引擎和结构化编辑器的全新产品。

本文着重强调墨干（Mogan Research）和GNU TeXmacs这两个同类产品的区别：

## 一表以蔽之
|  | GNU TeXmacs | 墨干 |
|--|------------|----------------|
| 发布周期 | 约一年 | 一个月至少一个版本 |
| 界面风格 | 复古风格 | 现代风格 | 
| 快捷键 | 部分失效 | 99%正常 |
| Scheme引擎 | Guile TeXmacs | S7 Scheme |
| 代码仓库 | SVN (Savannah) | Git (Codeberg/Gitee/Github) |
| 构建工具 | GNU Autotools | xmake | 
| 主导者 | Joris van der Hoeven (数学家) | 沈浪熊猫儿 (软件工程师) |


## 首选项
| 选项 | 墨干 | GNU TeXmacs|
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
### 快捷键前缀`structured:cmd`和`structured:insert`
结构化命令键前缀主要和切换折叠状态、切换编号状态、结构化移动、结构化删除、结构化选择等功能的快捷键相关。

在Windows和Linux平台，结构化命令键前缀从`Alt`切换到了`Ctrl`，结构化插入键前缀从`Meta`切换到了`Alt`。

在macOS平台，结构化命令键前缀从`Option`切换到了`Ctrl`，结构化插入键前缀从`Ctrl`切换到了`Option`。

> 为什么：通过切换结构化快捷键的两个键前缀，避免和和操作系统内置的快捷键冲突。比如`Meta+Left`在Windows上是Windows系统快捷键，`Ctrl+Left`在macOS上是macOS系统快捷键。


### 结构化变元轮换（自V1.2.3起）
全平台新增`A-S-up`和`A-S-down`这两个快捷键，用于在结构化变元之间切换。

在Windows和Linux平台仍然保留`C-tab`和`C-S-tab`的旧快捷键。在macOS平台，由于已经将`structured:cmd`从`Option`切换为了`Ctrl`，而且`C-tab`不可用，故而旧快捷键不生效。

> 为什么：因为`C-tab`/`C-S-tab`/`A-S-tab`这三个快捷键在macOS平台不可用，详见[QTBUG-12232](https://bugreports.qt.io/browse/QTBUG-12232)。(__注__: 该理由无效，因为最新TeXmacs测试版已升级至Qt6且不存在此类问题。)

### ESC键
在TeXmacs中，ESC键可用于模拟修饰键（比如Ctrl/Alt），但是在墨干中，ESC键是普通的按键，无法用于模拟修饰键。未来，ESC键或许可以被用于墨干的Vim插件。如果墨干中的快捷键和操作系统的内置快捷键冲突，我们会认为这是一个错误，会通过更改此类快捷键的方式修复错误，而不是用ESC键来规避错误。

## 绘图模式
| 菜单项 | 墨干 | GNU TeXmacs |
|--------|------------------|-----------|
| 插入`->`图像`->`绘制图形 | 显示网格 | 不显示网格 |
| 插入`->`图像`->`在当前焦点处绘制 | 显示网格 | 不显示网格 |
| 插入`->`网格`->`单位长度 | 2 | 1 |

## 插件
绝大部分GNU TeXmacs中的插件不再是墨干的内置插件。只有Maxima会话插件和Octave会话插件保留下来作为墨干的内置插件。

插件的概念在墨干中有扩充，参考[插件概述](plugins.md)，比如自然语言相关的字典和样式以语言插件的形式维护。

插件中心将在未来版本的墨干中可用，方便用户一键安装社区插件。

## TEXMACS_HOME_PATH
| 操作系统 | 墨干 | GNU TeXmacs |
|---------|-----|-------------|
| Linux   | `$XDG_DATA_HOME/XmacsLabs` | `$HOME/.TeXmacs` |
|         | `$HOME/.local/share/XmacsLabs` |  |
| macOS | `$HOME/Library/Application Support/XmacsLabs` | `$HOME/.TeXmacs` |
| Windows | `%APPDATA%\XmacsLabs` | `%APPDATA%\TeXmacs`|
| | `C:\Users\用户名\AppData\Roaming\XmacsLabs` | `C:\Users\用户名\AppData\Roaming\TeXmacs` |

> 为什么：不同的路径是为了在系统上可以同时安装墨干和GNU TeXmacs，另外Linux和macOS的路径更改是为了符合Linux和macOS相关标准。

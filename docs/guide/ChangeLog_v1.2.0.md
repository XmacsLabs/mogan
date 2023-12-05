# Mogan STEM Suite v1.2.0
Since v1.2.0, Mogan Editor has been renamed to Mogan STEM Suite.

Mogan STEM Suite v1.2.0:
+ Mogan Research v1.2.0

Mogan Research is the only product in Mogan STEM Suite for now. And Mogan Code and Mogan Beamer will be released.

## Major Changes
Compared with GNU TeXmacs 2.1.2:
+ OSPP Project `Mogan Draw on wasm`:
  + Improved the user experiences
  + Make it work via wasm in web browsers
+ OSPP Project `Editable PDF`:
  + 新增导出PDF并将tm文档作为附件嵌入的功能
  + 可以直接使用墨干打开带有tm文档附件的PDF文档
+ New menu entry for the community: Help->Planet
+ Upgrade to Qt 6.5.3
+ Adopt S7 Scheme as the Scheme engine to improve performance
+ Adopt KDE Breeze icons to beautify the UI
+ Fixed many dead shortcuts on Windows and macOS
+ 大量用户界面的细节改进：比如显示可用快捷键、调整界面翻译等等
+ 若干中文排版的重要改进：比如中英文之间自动插入很小的间隔等等
+ 若干字体相关重要改进：比如设置Linux平台默认中文字体等等
+ Several improvements for Biliography and Table
+ 调整若干默认配置项，改善用户体验
+ 修复若干导致墨干直接崩溃或者卡死的错误
+ Adjust the path of TEXMACS_HOME_PATH on Linux/macOS/Windows
+ Remove the built-in docs, load the latest GNU TeXmacs online docs
+ Experimental brower-based Mogan Research on wasm

## Know Issues
+ It will crash when clicking `Edit->Keyboard->Edit keyboard shortcuts`
+ For the first installation, it is very slow to open the app because it is loading all the fonts

v1.2.1 will be released on 2024/01/01 to solve the above issues.

## Changes in detail for end users
+ OSPP：Mogan Draw on wasm (Project 23，27，50)
  + 新增绘制`椭圆`和`扇形`的功能，焦点工具栏没有配置相关图标，可以使用`插入`菜单插入
  + Draw the circle via two points but not three points
  + 复制粘贴一个绘图对象时，新对象会在原对象的位置上少量偏移，避免重合
  + 按下Shift再单击，效果等同于右键，方便不使用鼠标只使用触摸板的场合下绘图
  + 旋转或者放缩时，将鼠标变成小手的形状
+ User Interface (Project 69，71)
  + Add menu entries to copy and paste in the context menu
  + 修复数学模式下`插入->高次方根式`失效的问题
  + Use the term `macOS` instead of `Mac OS`
  + 退出墨干的弹出窗口中，开启对于Cancel的翻译
  + 在模式工具栏中，直接使用语言而不是国旗显示当前的文档语言
+ Keyboard (Project 20, 59, 71)
  + Show shortcuts in the pull down menu of the focus toolbar
  + 显示模式工具栏中标号、折叠等图标的快捷键
  + 修复`插入->数学->多行公式`的快捷键提示
  + 新增用于删除一个单词`(kill-word)`的Emacs风格快捷键
  + New shortcut `std V`（`Ctrl+Shift+v` or `Command+Shift+v`）to paste verbatim
  + 幻灯片模式下，交换`下一个`和`下一屏`的快捷键，方便使用翻页笔展开当前屏幕的折叠内容
  + 文本模式下，将下上标、下标、上划线、下划线的快捷键调整为Tab循环风格的快捷键
  + 文本模式下，将列表的快捷键`Option+e`和`Option+i`改为Markdown风格的快捷键，避免和macOS系统快捷键冲突
  + Windows平台下，`编辑`菜单中的复制、粘贴、剪切的快捷键提示改为`C-c`，`C-v`, `C-x`
  + Linux和Windows平台下，Emacs风格的meta从Windows键调整为Alt，避免和系统快捷键冲突
  + macOS快捷键风格中，结构化插入的快捷键前缀从Ctrl改为Option，避免和macOS系统快捷键冲突
  + macOS快捷键风格中，将`cmd`键从`Ctrl`改为`Option`，比如`插入->节`从`Ctrl+1`变为`Option+1`
  + macOS快捷键风格中，将插入符号的快捷键从`Ctrl+q`改为`Option+q`，避免Qt 6无法识别`Ctrl+q`的问题
+ Preferences (Project 10)
  + `通用->显示询问`调整为`通过弹出窗口`
  + `其它->执行文档更新`调整为`三次`
  + 进入绘图模式（插入绘图区域或者在当前位置绘制）默认显示网格，可使用快捷键`#`切换
  + 绘图模式的网格默认单位长度从1调整为2
  + 默认开启`转换->PDF->展开幻灯片中的可折叠对象`
+ Stability (Project 7)
  + 修复内存少量泄露的问题
  + 修复将比较大的数转换为罗马数字崩溃的问题
  + 修复版本工具中使用粗粒度做文档比较崩溃的问题
  + Linux平台下，修复错误的环境变量LC_PAPER导致崩溃的问题

## Changes for Developers
+ 使用xmake 2.8.5作为构建工具，并配置了Ubuntu/macOS/Windows三个平台的CI和CD
+ 在Git仓库的devel目录下使用tm文档做项目管理，确立了Git提交信息的规范
+ 在`开发者`菜单中可以直接点击查看当前版本的项目管理，也可以直接打开错误模版和特性模版
+ Windows平台下，使用msvc而不是mingw来构建整个软件
+ HTML和LaTeX的导入导出、各种界面语言的翻译词典、编程语言的高亮初步插件化
+ TeXmacs的基础代码重构为Lolly项目作为Mogan的依赖
+ 在Lolly项目中引入tbox依赖解决棘手的字符编码以及跨平台的文件访问等问题


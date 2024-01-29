# 墨干理工套件V1.2.3
墨干理工套件V1.2.3包含以下组件：
+ 墨干V1.2.3 (Mogan Research v1.2.3)

## 重要变更
+ 提升了性能
+ 修复了大量错误
+ 改进了界面交互体验

## 影响用户体验的详细变更
+ 用户界面
  + 在Windows和Linux平台，也可以使用`工具->键盘->显示键盘按键`了
  + 修复macOS下外接非hidpi显示屏无法打开的问题
  + 修复macOS平台无权限访问用户`$HOME/Document`目录问题
+ 编辑器
  + 新增`std V`（`Ctrl+Shift+v`或者`Command+Shift+v`）用于粘贴纯文本
  + 新增`编辑->无格式粘贴`菜单项
  + 移除`编辑`->`复制/粘贴/剪切`的`Primary/Secondary/Ternary`菜单项
  + macOS：采用`Command+left`或者`Command+right`逐词移动
  + 采用Markdown风格的快捷键插入hlink（`[ ] var`）和slink（`[ ] var var`）
  + 修复在macOS平台无法使用`Command+minus`放大的问题
  + 修复光标在行首时概率出现（尤其是老电脑）的输入法漏字问题
  + 修复在拼音输入法下无法输入破折号的问题
+ 字体
  + 改进对思源宋体（Noto CJK fonts）的支持
  + 如果思源宋体可用，则默认的中日韩字体是思源宋体
  + 支持CESI（中国电子技术标准化研究院）的宋体、黑体、楷体、仿宋、小标宋
+ 插件
  + Maxima: 修复输入卡顿的问题
  + Maxima: 工具栏上的帮助按钮直接访问官网文档
  + 拼写检查: 在Windows和Linux开启`编辑->拼写检查`
  + 拼写检查: 修复没有安装hunspell或者aspell时，在Windows平台做拼写检查会崩溃的问题
  + LaTeX: 修复复制多行公式到LaTeX代码片段出现乱码的问题

## 开发者相关变更
+ 使用`xmake-requires.lock`机制以改善构建的可复现性
+ 在macOS arm平台的安装包已经由Github Action自动打包了

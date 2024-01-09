# 墨干理工套件V1.2.3
## 影响用户体验的详细变更
+ 编辑器
  + 新增`std V`（`Ctrl+Shift+v`或者`Command+Shift+v`）用于粘贴纯文本
  + 新增`编辑->无格式粘贴`菜单项
  + 移除`编辑`->`复制/粘贴/剪切`的`Primary/Secondary/Ternary`菜单项
  + 修复光标在行首时概率出现（尤其是老电脑）的输入法漏字问题
  + macOS：采用`Command+left`或者`Command+right`逐词移动
  + 采用Markdown风格的快捷键插入hlink（`[ ] var`）和slink（`[ ] var var`）
+ 字体
  + 改进对思源宋体（Noto CJK fonts）的支持
  + 如果思源宋体可用，则默认的中日韩字体是思源宋体
  + 支持CESI（中国电子技术标准化研究院）的宋体、黑体、楷体、仿宋、小标宋

## 开发者相关变更
+ 使用`xmake-requires.lock`机制以改善构建的可复现性

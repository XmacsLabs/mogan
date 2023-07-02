# 墨干编辑器V1.0.2
## 什么是GNU TeXmacs
[A Quick Tour of GNU TeXmacs](https://www.zhihu.com/zvideo/1399501992717332480)

## 主要变更
+ 全新的图标
+ 文档入口`帮助->墨干`直接加载远程文档
+ 错误修复
  - LaTeX的导入导出
  - 分段函数的排版错乱问题
  - 修复gnuplot会话在折叠模式下的错误

## 温馨提示
### 请新用户们谅解
第一次打开墨干编辑器会比较慢，属于正常现象。目前暂时没有精力做第一次用户的引导界面。

### 给老用户们的温馨提示
如果你点击菜单`Help->Mogan`或者`帮助->墨干`弹出报错对话框，可以按照如下方式清楚历史版本的缓存。

以Windows平台为例，参考[如何在Windows中重置GNU TeXmacs?](https://jingyan.baidu.com/article/75ab0bcb08286d97864db2db.html) ，注意将用户目录改为`%appdata%/Xmacs`。macOS和GNU/Linux用户，可以使用命令行`rm -rf $HOME/.Xmacs/system/`重置墨干编辑器。
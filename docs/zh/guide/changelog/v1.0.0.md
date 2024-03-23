# 墨干编辑器V1.0.0
## 发布
墨干编辑器衍生自GNU TeXmacs（作者是Joris van der Hoeven）。与TeXmacs的主要区别是：
+ 界面：从Qt 4.x升级到了Qt 5.x
+ 脚本引擎：从Guile 1.8切换到了S7 (作者是Massimiliano Gubinelli)

墨干编辑器v1.0.0是基于GNU TeXmacs 2.1.1定制的基础版本，功能上和2.1.1完全一致。墨干编辑器v1.0.x将专注于错误修复、易用性改善和性能提升。

什么是GNU TeXmacs，请看: https://www.bilibili.com/video/BV19741167ik

## 特性一览 
### 默认配置优化
+ `编辑→首选项→通用→快捷键风格`分别从`macOS风格`和`Windows风格`切换成了`Emacs风格`
+ `编辑→首选项→通用→显示询问`从`在状态栏`切换到`在弹出窗口`
+ `编辑→首选项→通用→缓冲区管理`从`一个文档一个窗口`切换成`多个文档共享一个窗口`
+ `编辑→首选项→其它→执行文档更新`从`一次`切换成`三次`

### 快捷键优化
Windows平台的Meta快捷键默认是Win，多数和系统快捷键冲突，目前已经将Windows平台的Meta快捷键从Win切换为Alt，解决了大部分Windows平台快捷键失效的问题。

### 新增插件
+ quiver插件能够帮助你快速绘制交换图（感谢 @xuyiqi1 ）
+ pyenv插件能够帮助你切换Python会话所使用的Python虚拟环境

### 示例文档
此处提供示说网的PDF预览，在`帮助→墨干编辑器`中可以找到可编辑的原始文件。

+ [2019年全国高中数学联合竞赛一试试题A卷](https://www.slidestalk.com/u282/2019A39734)
+ [2021年普通高等学校招生全国统一考试：数学试题卷](https://www.slidestalk.com/u282/zjmath202182681)

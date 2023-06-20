# 什么是墨干编辑器
> **让我们享受探索科学与技术的乐趣！**

墨干编辑器是[墨客实验室](https://gitee.com/XmacsLabs)旗下的结构化编辑器。我们致力于将墨干编辑器打造成最易用的 [GNU TeXmacs](https://www.texmacs.org) 发行版。

GNU TeXmacs 是法国国家科学研究中心的数学家 [Joris van der Hoeven](http://www.texmacs.org/joris/main/joris.html) 从上世纪九十年代末开始研发的一款结构化编辑器，旨在利用直观易用的图形界面编写高质量的技术文档，效果可达到与 LaTeX 相同的水准。目前它的主要用户集中于中小学教师及高校科研人员。

## 视频: A quick tour of GNU TeXmacs
[![A quick tour of TeXmacs-0001](https://user-images.githubusercontent.com/32867606/198896005-72077867-bd0f-4223-9f87-099ec3815ba5.png)](https://player.bilibili.com/player.html?aid=376713018&bvid=BV1bo4y1D7wN&cid=371195201&page=1)

## 墨干编辑器的诞生
[沈浪熊猫儿](http://texmacs.org/tmweb/contribute/team-sadhen.en.html)于2022年1月30号，在 [Massimiliano Gubinelli](http://texmacs.org/tmweb/contribute/team-massimiliano.en.html) 的 S7 Scheme 分支的基础上，发布了[墨干编辑器v1.0.0](https://gitee.com/XmacsLabs/mogan/releases/tag/v1.0.0)，并创建了[墨客实验室](https://gitee.com/XmacsLabs)这样一个非盈利组织，旨在普及GNU TeXmacs，让所有人都能够通过 Joris van der Hoeven 创造的软件享受探索科学与技术的乐趣。

## 墨干编辑器与GNU TeXmacs的区别
两者主要区别如下：
- 合理的配置调优. 比如在墨干编辑器中默认的快捷键风格是 Emacs 风格，这样在不同平台快捷键是一致的。如果你想切换回 Ctrl-C 或者 Command-C 这种随系统不同变化的快捷键风格，可以在 编辑→首选项→通用→快捷键风格 中改回来。
- 激进的技术选型. 比如墨干编辑器将 GUI 框架从 Qt 4.8.x 切换到了 Qt 5.15.x，将脚本执行引擎从 Guile 1.8.x 切换到了 S7 Scheme，在享受新技术带来的优势的同时，也会有不少不稳定的因素。

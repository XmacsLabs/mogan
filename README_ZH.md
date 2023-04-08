# 墨干编辑器 / [Mogan Editor](README.md)
![GPL v3](http://www.gnu.org/graphics/gplv3-127x51.png)

> **让我们享受探索科学与技术的乐趣！**

墨干编辑器是[墨客实验室](https://gitee.com/XmacsLabs)旗下的结构化编辑器。我们致力于将墨干编辑器打造成最易用的 [GNU TeXmacs](https://www.texmacs.org) 发行版。

## 如何安装
Windows、macOS和Ubuntu这三大平台的安装包是由墨客实验室官方提供的。其他操作系统（比如Arch Linux, OpenSUSE, GNU Guix）的安装包由墨客社区维护。

+ 马上下载：[安装指南](docs/zh/guide/Install.md)
+ 在线试用：[实验性的WASM移植版](https://mogan.app/wasm/Mogan.html)

## 什么是 GNU TeXmacs 以及墨干编辑器
GNU TeXmacs 是法国国家科学研究中心的数学家 [Joris van der Hoeven](http://www.texmacs.org/joris/main/joris.html) 从上世纪九十年代末开始研发的一款结构化编辑器，旨在利用直观易用的图形界面编写高质量的技术文档，效果可达到与 LaTeX 相同的水准。目前它的主要用户集中于中小学教师及高校科研人员。

[沈浪熊猫儿](http://texmacs.org/tmweb/contribute/team-sadhen.en.html)于2022年1月30号，在 [Massimiliano Gubinelli](http://texmacs.org/tmweb/contribute/team-massimiliano.en.html) 的 S7 Scheme 分支的基础上，发布了[墨干编辑器v1.0.0](https://gitee.com/XmacsLabs/mogan/releases/tag/v1.0.0)，并创建了[墨客实验室](https://gitee.com/XmacsLabs)这样一个非盈利组织，旨在普及GNU TeXmacs，让所有人都能够通过 Joris van der Hoeven 创造的软件享受探索科学与技术的乐趣。

两者有如下主要区别：
- 合理的配置调优. 比如在墨干编辑器中默认的快捷键风格是 Emacs 风格，这样在不同平台快捷键是一致的。如果你想切换回 Ctrl-C 或者 Command-C 这种随系统不同变化的快捷键风格，可以在 编辑→首选项→通用→快捷键风格 中改回来。
- 激进的技术选型. 比如墨干编辑器将 GUI 框架从 Qt 4.8.x 切换到了 Qt 5.15.x，将脚本执行引擎从 Guile 1.8.x 切换到了 S7 Scheme，在享受新技术带来的优势的同时，也会有不少不稳定的因素。

### 特色功能一览：

[![A quick tour of TeXmacs-0001](https://user-images.githubusercontent.com/32867606/198896005-72077867-bd0f-4223-9f87-099ec3815ba5.png)](https://player.bilibili.com/player.html?aid=376713018&bvid=BV1bo4y1D7wN&cid=371195201&page=1)

- GNU TeXmacs 是一个免费的科技编辑器平台，提供了“所见即所得”的用户界面，可用于创建漂亮的技术文档。
- GNU TeXmacs 为不同类型的结构化文档内容（文本、数学、图像、交互式内容、幻灯片）提供了一致且优化的编辑体验。
- GNU TeXmacs 可用作各种系统的图形化前端，比如计算机代数系统、数值分析系统、统计分析系统
- 其文档以 GNU TeXmacs 的 tm 格式存储，可以导出为 PDF 或 Postscript 文档，内置 TeX/LaTeX 和 Html/Mathml 转换器。注意 TeXmacs 并不基于 TeX/LaTeX。
- 其渲染引擎高质量的排版算法可输出专业水准的文档，既可用于打印，也可用于在电脑屏幕上展示。
- 用户可以自定义排版样式，也可以使用 Scheme 扩展语言为编辑器添加新功能。
- 可以在所有主流 Unix 平台、macOS 和 Windows 上运行。

墨干继承了 GNU TeXmacs 以上所有特性（请参考 [GNU TeXmacs 主页](https://www.texmacs.org))，是理工科学生、中小学教师和研究人员学习工作的利器，可为创作高质量的作业文档、高水准的报告幻灯片和 LaTeX 水准的论文提供便利。基于 GNU TeXmacs，墨干改正了许多错误，优化了性能，并美化了界面。

![搭配 KDE Brezze 图标的墨干 v1.1.1](https://user-images.githubusercontent.com/32867606/198898710-6c16bfb2-1107-4cb0-8cb7-14606b31a315.png)

## 如何入门

### 入门视频教程
[**GNU TeXmacs for scientists**（YouTube）](https://youtube.com/playlist?list=PLjlnuLy3KEpYmUQvFIFSFgeYQjVYO2gxi)
 [GNU TeXmacs for scientists (Bilibili)](https://www.bilibili.com/video/BV1h4411w7xy/) / [TeXmacs 入门（Bilibili）](https://www.bilibili.com/medialist/play/617908?from=space&business=space_series&business_id=2765862&desc=1) / [TeXmacs 入门（知乎）](https://www.zhihu.com/collection/844048143)）作为入门教程。

### 用户指南
[**The Jolly Writer**](https://www.scypress.com/book_info.html) 是目前唯一的系统且详细的 TeXmacs 官方教材。请[点击此处](https://www.scypress.com/book_download.html)免费下载或捐赠。

### 其他资料
- GNU TeXmacs 的官方文档. 在帮助菜单下可以直接浏览。需要注意的一点是，如果界面语言是中文，那么已经完成翻译的帮助文档也是中文文档；如果界面语言是英文，那么帮助文档是英文的。
- 墨客星球. 简称星球，通过 `帮助 → 墨客星球` 可以直接访问。
- 视频资源. `视频集锦` 是星球中的文档，收录了大量的教学视频。

## 联系我们
衷心欢迎大家通过以下方式联系我们，交流经验、提供建议或报告问题。也热情欢迎感兴趣的开发者和志愿贡献者加入墨者实验室。

|||
| :---: | :---: |
墨干官网|[mogan.app](https://mogan.app)
Bilibili|[沈浪熊猫儿](https://space.bilibili.com/28058658) 
Gitee|[墨者实验室](https://gitee.com/XmacsLabs/) 
GitHub|[XmacsLabs](https://github.com/XmacsLabs)
豆瓣小组|[GNU TeXmacs](https://www.douban.com/group/texmacs/)
知乎|[墨干编辑器](https://www.zhihu.com/people/xmacs)
微信公众号|墨者实验室（xmacs2020）
微信群|墨客社区（通过账户 XmacsLabs 加入）
QQ群|GNU TeXmacs 中文社区（934456971）

使用时遇到问题也欢迎咨询 TeXmacs 社区，特别是官方论坛。

|||
| :---: | :---: |
GNU TeXmacs 官网|[texmacs.org](http://www.texmacs.org)
官方论坛|[The GNU TeXmacs forum](http://forum.texmacs.cn) 
YouTube|[TeXmacs Videos](https://www.youtube.com/channel/UCLaZZkOj3GPYFu9pVsEbthg)
GitHub|[GNU TeXmacs](https://github.com/texmacs) 
Mastodon|[GNU TeXmacs](https://mathstodon.xyz/@gnu_texmacs)
Twitter|[GNU TeXmacs](https://twitter.com/gnu_texmacs) 
Gitter|[texmacs/Lobby](https://gitter.im/texmacs/Lobby) 
邮件列表|[Mailing lists for TeXmacs](https://www.texmacs.org/tmweb/home/ml.en.html) 

## 墨客星球 
墨客星球是一个由墨干编辑器用户提交的 TeXmacs 文件的汇总，通过 `帮助 → 墨客星球` 可以直接访问。墨客星球的主题专注科学和技术。

## 关于名字“墨干”
[沈浪熊猫儿](http://texmacs.org/tmweb/contribute/team-sadhen.en.html)：
> 墨干得名于家乡的避暑胜地——[莫干山](https://cn.wikipedia.org/wiki/Mount_Mogan)。犹忆青春年少时，在高中同桌老家，靠近莫干山的一个小山村里面，住过一晚上。傍晚，我们就在屋后小溪里面洗澡。清晨，我们跟着他在山上兜兜转转挖笋。中午，一起吃刚挖好的笋。溪水潺潺，篁竹幽幽，炊烟袅袅……

## 如何贡献
请仔细阅读[贡献指南](https://mogan.app/zh/guide/CONTRIBUTING.html)，以了解如何开始为墨干编辑器做贡献。

## 贡献者
<table>
  <tr>
    <td align="center"><a href="http://texmacs.org/tmweb/contribute/team-massimiliano.en.html"><img src="https://avatars.githubusercontent.com/u/3253062?v=4?s=50" width="50px;" alt=""/><br /><sub><b>Massimiliano Gubinelli</b></sub></a><br />💻</td>
    <td align="center"><a href="http://texmacs.org/tmweb/contribute/team-sadhen.en.html"><img src="https://avatars.githubusercontent.com/u/1267865?v=4?s=50" width="50px;" alt=""/><br /><sub><b>DarcyShen</b></sub></a><br />💻</td>
    <td align="center"><a href="https://github.com/YiqiXu"><img src="https://avatars.githubusercontent.com/u/32867606?v=4?s=50" width="50px;" alt=""/><br /><sub><b>YiqiXu</b></sub></a><br />📖</td>
    <td align="center"><a href="https://github.com/PikachuHy"><img src="https://avatars.githubusercontent.com/u/18223871?v=4?s=50" width="50px;" alt=""/><br /><sub><b>PikachuHy</b></sub></a><br />💻</td>
    <td align="center"><a href="https://github.com/jingkaimori"><img src="https://avatars.githubusercontent.com/u/36156959?v=4?s=50" width="50px;" alt=""/><br /><sub><b>jinkaimori</b></sub></a><br />💻</td>
    <td align="center"><a href="https://github.com/Yufeng-shen"><img src="https://avatars.githubusercontent.com/u/17488004?v=4?s=50" width="50px;" alt=""/><br /><sub><b>YufengShen</b></sub></a><br />💻</td>
  </tr>
  <tr>
    <td align="center"><a href="https://github.com/KingJadeLight"><img src="https://avatars.githubusercontent.com/u/106169959?v=4?s=50" width="50px;" alt=""/><br /><sub><b>KingJadeLight</b></sub></a><br />💻</td>
    <td align="center"><a href="https://github.com/luozhiya"><img src="https://avatars.githubusercontent.com/u/90168447?v=4?s=50" width="50px;" alt=""/><br /><sub><b>luozhiya</b></sub></a><br />💻</td>
    <td align="center"><a href="https://github.com/pan93412"><img src="https://avatars.githubusercontent.com/u/28441561?v=4?s=50" width="50px;" alt=""/><br /><sub><b>pan93412</b></sub></a><br />💻</td>
    <td align="center"><a href="https://github.com/SpecterShell"><img src="https://avatars.githubusercontent.com/u/56779163?v=4?s=50" width="50px;" alt=""/><br /><sub><b>SpecterShell</b></sub></a><br />💻</td>
    <td align="center"><a href="https://github.com/iphelf"><img src="https://avatars.githubusercontent.com/u/17234854?v=4?s=50" width="50px;" alt=""/><br /><sub><b>LingfengFu</b></sub></a><br />💻</td>
    <td align="center"><a href="https://github.com/ice1000"><img src="https://avatars.githubusercontent.com/u/16398479?v=4?s=50" width="50px;" alt=""/><br /><sub><b>TeslaZhang</b></sub></a><br />💻</td>
  </tr>
  <tr>
    <td align="center"><a href="https://github.com/cqyisbug"><img src="https://avatars.githubusercontent.com/u/25382172?v=4?s=50" width="50px;" alt=""/><br /><sub><b>QiyuanCai</b></sub></a><br />💻</td>
    <td align="center"><a href="https://github.com/cireu"><img src="https://avatars.githubusercontent.com/u/18224684?v=4?s=50" width="50px;" alt=""/><br /><sub><b>ZihaoZhu</b></sub></a><br />💻</td>
  </tr>
</table>

# Mogan Editor / [墨干编辑器](README_ZH.md)
![GPL v3](http://www.gnu.org/graphics/gplv3-127x51.png)

##Fork repository to develop the "Mogan Draw on WASM" ospp project

> **Let us enjoy exploring science and technology!**

Mogan Editor is a structure editor delivered by [Xmacs Labs](https://github.com/XmacsLabs). We are dedicated to make Mogan Editor the most user-friendly distribution of [GNU TeXmacs](https://www.texmacs.org).

## How to install
Windows, macOS and Ubuntu installers are officially provided. Packages for other OS (eg. Arch Linux, OpenSUSE, GNU Guix) are maintained by the community.

+ Download now via [Installation Guide](docs/guide/Install.md)
+ Try online via the [Experimental WASM port](https://mogan.app/wasm/Mogan.html)

## What are GNU TeXmacs and Mogan Editor
GNU TeXmacs is a structured editor developed since the late 1990s by [Joris van der Hoeven](http://www.texmacs.org/joris/main/joris.html), a mathematician at CNRS in France. It is designed to write high-quality technical documents using an intuitive and easy-to-use graphical interface, and able to achieve the same level of results as LaTeX. Its main users are currently primary and secondary school teachers and university researchers.

[Darcy Shen](http://texmacs.org/tmweb/contribute/team-sadhen.en.html) released [Mogan Editor v1.0.0](https://github.com/XmacsLabs/mogan/releases/tag/v1.0.0) on January 30, 2022, based on [Massimiliano Gubinelli](http://texmacs.org/tmweb/contribute/team-massimiliano.en.html)'s [S7 Scheme port](https://lists.gnu.org/archive/html/texmacs-dev/2021-01/msg00009.html), and created Xmacs Labs, a non-profit organization that aims to popularize GNU TeXmacs and make it available for everyone to enjoy exploring science and technology via the excellent software developed by Joris van der Hoeven.

The main differences from GUN TeXmacs:
- Reasonable configuration tuning.  For example, the default shortcut style in Mogan Editor is Emacs style, so that the shortcut keys are consistent across platforms. If you want to switch back to the Ctrl-C or Cmd-C shortcut style which varies with the system, you can change it back in Edit→Preferences→General→Look and feel.
- Radical technical options. For example, Mogan Editor switched the GUI framework from Qt 4.8.x to Qt 5.15.x, and switched the script execution engine from Guile 1.8.x to S7 Scheme, which can be quite unstable while enjoying the advantages of new technology.


### Features at a Glance

[![A quick tour of TeXmacs-0001](https://user-images.githubusercontent.com/32867606/198896005-72077867-bd0f-4223-9f87-099ec3815ba5.png)](https://youtu.be/H46ON2FB30U)

- GNU TeXmacs is a free scientific editing platform designed to create beautiful technical documents using a wysiwyg interface.
- It provides a unified and user friendly framework for editing structured documents with different types of content: text, mathematics, graphics, interactive content, slides, etc.
- TeXmacs can be used as a graphical front-end for many systems in computer algebra, numerical analysis, statistics, etc.
- Documents can be saved in TeXmacs, Xml or Scheme format and printed as Pdf or Postscript files. Converters exist for TeX/LaTeX and Html/Mathml. Notice that TeXmacs is not based on TeX/LaTeX.
- Its rendering engine uses high-quality typesetting algorithms so as to produce professionally looking documents, which can either be printed out or presented from a laptop.
- New styles can be written by the user and new features can be added to the editor using the Scheme extension language.
- Runs on all major Unix platforms, macOS, and Windows.

Mogan inherits all the above features from GNU TeXmacs (see [the homepage of GNU TeXmacs](https://www.texmacs.org)), so it can also be used as a efficient study tool for students, teachers, and researchers in science and technology, and suitable to create high-quality assignment documents, report slides, and LaTeX-level papers. Based on GNU TeXmacs, Mogan has more bugs fixed, performance-optimized, and the interface beautified.

![SCR-20221030-sve](https://user-images.githubusercontent.com/32867606/198899063-e8a4aab4-176f-4e2c-94e3-20656984f39b.png)

## Getting Started

### Video Tutorials
[**GNU TeXmacs for scientists**（YouTube）](https://youtube.com/playlist?list=PLjlnuLy3KEpYmUQvFIFSFgeYQjVYO2gxi)

### User's Guide
[**The Jolly Writer**](https://www.scypress.com/book_info.html) is the only systematic and detailed official TeXmacs textbook available. Please [click here](https://www.scypress.com/book_download.html) to download for free or make a donation.

## Contact Us
You are sincerely welcome to contact us via the following ways to share your experience, provide suggestions or report issues on Mogan Editor. Interested developers and volunteer contributors are also warmly welcome to join Xmacs Labs.

|||
| :---: | :---: |
Official Website of Mogan|[mogan.app](https://mogan.app)
Bilibili|[沈浪熊猫儿](https://space.bilibili.com/28058658) 
Gitee|[墨者实验室](https://gitee.com/XmacsLabs/)
GitHub|[XmacsLabs](https://github.com/XmacsLabs)
Douban Group|[GNU TeXmacs](https://www.douban.com/group/texmacs/)
Zhihu|[墨干编辑器](https://www.zhihu.com/people/xmacs)
WeChat Official Account|墨者实验室（xmacs2020）
WeChat Group|墨客社区（Join via account XmacsLabs）
QQ Group|GNU TeXmacs 中文社区（934456971）

You are also welcome to consult the TeXmacs community, especially the official forum, if you encounter problems while using Mogan.

|||
| :---: | :---: |
Official Website of GNU TeXmacs|[texmacs.org](http://www.texmacs.org)
Official Forum|[The GNU TeXmacs forum](http://forum.texmacs.cn) 
YouTube|[TeXmacs Videos](https://www.youtube.com/channel/UCLaZZkOj3GPYFu9pVsEbthg)
GitHub|[GNU TeXmacs](https://github.com/texmacs) 
Mastodon|[GNU TeXmacs](https://mathstodon.xyz/@gnu_texmacs)
Twitter|[GNU TeXmacs](https://twitter.com/gnu_texmacs) 
Gitter|[texmacs/Lobby](https://gitter.im/texmacs/Lobby) 
Mailing Lists|[Mailing lists for TeXmacs](https://www.texmacs.org/tmweb/home/ml.en.html) 

## Xmacs Planet 
Xmacs Planet is an aggregation of TeXmacs documents submitted by Mogan Editor users, and it can be accessed through `Help → Planet`. The topic of Xmacs Planet focuses on Science and Technology.

## The Name "Mogan"
[Darcy Shen](http://texmacs.org/tmweb/contribute/team-sadhen.en.html):
> Mogan is named after [Mount Mogan](https://en.wikipedia.org/wiki/Mount_Mogan), a famous attraction in my hometown. 墨干(Mogan) literally means "dried ink". The word is derived from 莫干(Mogan) which is a combination of [莫(Mo) and 干(Gan)](https://en.wikipedia.org/wiki/Gan_Jiang_and_Mo_Ye).

## Contributing as User or Developer
Please review the [Contribution guide](https://mogan.app/guide/CONTRIBUTING.html) for information on how to get started contributing to Mogan Editor.

## Contributors
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


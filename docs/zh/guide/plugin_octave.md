# Octave 插件

Octave 插件在墨干 V1.2.5 LTS 中内置，自墨干 v1.2.7 起不再内置。

相关教学视频：[零基础墨干 08：用 Octave 插件学线性代数](https://www.bilibili.com/video/BV1gK421a7CK/)

## 安装

Octave 插件依赖于[Ghostscript 二进制插件](plugin_binary_gs.md)，请先按照文档安装 Ghostscript，然后查看`帮助->插件->Octave`以确保文档中的绘图可以正确渲染。

### Windows

前往[Octave 官网](https://octave.org)，下载并安装 Octave 5.2.0。

比如下载由中国科学技术大学开源软件镜像站提供的 Octave 5.2.0 安装包：

- http://mirrors.ustc.edu.cn/gnu/octave/windows/octave-5.2.0_1-w64-installer.exe
- http://mirrors.ustc.edu.cn/gnu/octave/windows/octave-5.2.0_1-w64-installer.exe.sig

### macOS

```
brew install octave
```

### Linux

在 Debian 及其衍生中，使用如下命令安装：

```
sudo apt install octave
```

## 小贴士

1. Windows：Octave 6.x 及以上版本的绘图功能在墨干中无法正常使用
   - 未来会让墨干支持 Octave 的新版的绘图，也欢迎大家贡献代码
2. Ubuntu: 使用 snap 安装的 Octave 无法在墨干中正常使用
   - 这是由 snap 的机制决定的，故而不建议使用 snap 安装 Octave

## 内置文档见`帮助->插件->Octave`

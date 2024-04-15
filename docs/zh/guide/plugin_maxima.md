# Maxima会话插件
相关教学视频：[零基础墨干09：用Maxima插件学微积分](https://www.bilibili.com/video/BV1JJ4m1V7Mq/)

## 安装
Maxima会话插件依赖于[Ghostscript二进制插件](plugin_binary_gs.md)，请先按照文档安装Ghostscript，然后查看`帮助->插件->Maxima`以确保文档中的绘图可以正确渲染。

### Windows
从[Maxima官网](https://maxima.sourceforge.io)下载并安装。

推荐的Maxima版本是Maxima 5.47.0。

### macOS
```
brew install maxima
```
### Linux
在Debian及其衍生中，使用如下命令安装：
```
sudo apt install maxima
```
## 小贴士
+ Ubuntu: 使用snap安装的Maxima无法在墨干中正常使用
  - 这是由snap的机制决定的，故而不建议使用snap安装Maxima

## 文档：请查看`帮助->插件->Maxima`
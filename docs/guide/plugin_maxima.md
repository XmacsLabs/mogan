# Maxima Session Plugin
Video Tutorial in Chinese: [零基础墨干09：用Maxima插件学微积分](https://www.bilibili.com/video/BV1JJ4m1V7Mq/)

## Installation
The Maxima session plugin relies on the [Ghostscript Binary plugin](plugin_binary_gs.md). Please follow the documentation to install Ghostscript first, and then check `Help -> Plugins -> Maxima` to ensure that the PDF images in the documentation can be rendered correctly.

### Windows
Download and install from the [Maxima official website](https://maxima.sourceforge.io).

The recommended version of Maxima is 5.47.0.

### macOS
```
brew install maxima
```

### Linux
On Debian and its derivatives, use the following command to install:
```
sudo apt install maxima
```

## Tips
+ Ubuntu: Maxima installed using snap cannot work with Mogan.
  - This is due to the mechanism of snap, so it is not recommended to install Maxima using snap.

## Check built-in Doc at `Help -> Plugins -> Maxima`

# How to install
## GNU Guix
```
guix install mogan
```
更多信息：https://packages.guix.gnu.org/packages/mogan/

> 注意：可能会遇到中文菜单无法正常显示的问题，这并不是墨干编辑器本身的问题，您可能需要花一些时间学习如何配置Guix的中文支持。

## Windows

> 说明：GNU TeXmacs是独立于LaTeX/TeX实现的，不需要再安装TeXLive。

### 手动
在[Gitee](https://gitee.com/XmacsLabs/mogan/releases)下载安装包，然后双击按照提示安装即可。

### winget
```
winget install -e --id XmacsLabs.Mogan
```
关于winget的更多信息: https://learn.microsoft.com/zh-cn/windows/package-manager/winget/

> 注意，winget会从Github下载安装包，如果网络有问题，还是建议手动下载安装。

## macOS
在[Gitee](https://gitee.com/XmacsLabs/mogan/releases)下载安装包，然后按照macOS平台通用的步骤安装即可。因为我们没有购买苹果的开发者认证，所以需要在苹果系统的安全设置中选择仍然打开。

## GNU/Linux

### Arch Linux (AUR)
```bash
yay -S mogan
```
更多信息：https://aur.archlinux.org/packages/mogan

### openSUSE (OBS)

使用系统默认的软件包管理器`zypper`：

```bash
zypper addrepo https://download.opensuse.org/repositories/home:iphelf/openSUSE_Tumbleweed/home:iphelf.repo
zypper refresh
zypper install Mogan
```

或者，使用OBS软件包安装器[`opi`](https://software.opensuse.org/package/opi)：

```bash
opi Mogan
```

更多信息：https://software.opensuse.org/download/package?package=Mogan&project=home%3Aiphelf



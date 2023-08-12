# 如何安装
## 官方提供的安装包
| 操作系统 | 马上下载 | MD5校验 |
|-------|-----|--------|
| Windows (>=10)| [⏬ 下载](https://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/v1.1.5/Mogan-v1.1.5-64bit-installer.exe) | 7c53df195ddc66fd9c84026100caa066 |
| macOS (>=11) | [⏬ 下载](https://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/v1.1.5/Mogan_v1.1.5.dmg) | feef723476a32a3d0a85ec21b9e400d2 |
| macOS arm (>=13.1) | [⏬ 下载](https://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/v1.1.5/Mogan_arm_v1.1.5.dmg) | e09d6d99f27a53589c6a9323de1ff9be |
| Ubuntu 20.04 | [⏬ 下载](https://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/v1.1.5/mogan-v1.1.5-ubuntu20.04.deb) | bc8bec1a078a98530d25ee9465fb2a75 |
| Ubuntu 22.04 | [⏬ 下载](https://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/v1.1.5/mogan-v1.1.5-ubuntu22.04.deb) | 5b00b931b4864222d582a4e11a7e25d0 |
| UOS amd64 | [⏬ 下载](https://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/v1.1.5/mogan-v1.1.5-uos.deb) | 882743a622b8a71cbbec0bc98af1cfe9 |
| UOS loongarch | [⏬ 下载](https://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/v1.1.5/mogan-v1.1.5-uos-loongarch64.deb) | 9bece35cfbf2d175455fdc6841c36475 |

从其它站点下载：
+ https://codeberg.org/XmacsLabs/mogan/releases
+ https://gitee.com/XmacsLabs/mogan/releases
+ https://github.com/XmacsLabs/mogan/releases

旧版本安装包：
+ [v1.1.4 (2023/07/31)](https://github.com/XmacsLabs/mogan/releases/tag/v1.1.4)
+ [v1.1.3 (2023/06/05)](https://github.com/XmacsLabs/mogan/releases/tag/v1.1.3)
+ [v1.1.2 (2023/04/09)](https://gitee.com/XmacsLabs/mogan/releases/tag/v1.1.2)
+ [v1.1.1 (2022/10/29)](https://gitee.com/XmacsLabs/mogan/releases/tag/v1.1.1)
+ [v1.1.0 (2022/08/31)](https://gitee.com/XmacsLabs/mogan/releases/tag/v1.1.0)


## GNU Guix
```
guix install mogan
```
更多信息：https://packages.guix.gnu.org/packages/mogan/

> 注意：可能会遇到中文菜单无法正常显示的问题，这并不是墨干编辑器本身的问题，您可能需要花一些时间学习如何配置Guix的中文支持。

## Windows
```
winget install -e --id XmacsLabs.Mogan
```
关于winget的更多信息: https://learn.microsoft.com/zh-cn/windows/package-manager/winget/

> 注意，winget会从Github下载安装包，如果网络有问题，还是建议手动下载安装。

## Arch Linux (AUR)
```bash
yay -S mogan
```
更多信息：https://aur.archlinux.org/packages/mogan

## openSUSE (OBS)

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

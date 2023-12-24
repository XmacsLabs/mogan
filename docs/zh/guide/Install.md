# 如何安装
当前最新稳定版是墨干理工套件v1.2.2，可以在`帮助 -> 欢迎`这个菜单项中检查是否有新版可用。

## 官方提供的安装包
| 产品 | 操作系统 | 马上下载 |
|-----|-------|-----|
| 墨砚v1.2.2 | Windows (>=10)| [⏬ 下载](https://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/v1.2.2/MoganResearch-v1.2.2-64bit-installer.exe) |
| 墨砚v1.2.2 | macOS (>=11) | [⏬ 下载](https://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/v1.2.2/MoganResearch-v1.2.2.dmg) |
| 墨砚v1.2.2 | macOS M1/M2 (>=13) | [⏬ 下载](https://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/v1.2.2/MoganResearch-v1.2.2-arm.dmg) |
| 墨砚v1.2.2 | Ubuntu 22.04 | [⏬ 下载](https://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/v1.2.2/mogan-research-v1.2.2-ubuntu22.04.deb) |
| 墨砚v1.2.2 | UOS | [⏬ 下载](https://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/v1.2.2/mogan-research-v1.2.2-uos.deb) |
| 墨砚v1.2.2 | UOS Loongarch | [⏬ 下载](https://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/v1.2.2/mogan-research-v1.2.2-uos-loongarch64.deb) |

SHA256校验码：
```
8b8e2cc54c64436b965417c9f76beaded181d3c4a192f2eae62189d88b754e28  MoganResearch-v1.2.2-64bit-installer.exe
ccbd7843542cd94c4a5315694c62c7f5dbdb81e66542820d649e1328c42fc8b5  MoganResearch-v1.2.2-arm.dmg
6470306b334f2da09a6be515ba2189c639954ae53333c0213074154364d137e7  MoganResearch-v1.2.2.dmg
dc9f3d1e18afb4f27b598c8251815ad1082f14c58e6685e218965c63e6d19151  mogan-research-v1.2.2-ubuntu22.04.deb
5185047d2acbf83465ec5d28406133512a7b8c6d450b70d08062b34c9dc59af1  mogan-research-v1.2.2-uos-loongarch64.deb
9ae3f63c7b562fc51a408d97ad3f253fe11aa30a348f8212e277b3e1d5a1bc10  mogan-research-v1.2.2-uos.deb
```

墨干理工套件：
+ 墨砚（提供Windows/macOS/Ubuntu/UOS四大平台安装包）
+ 墨码（无安装包，正在开发中）
+ 墨板（无安装包，正在开发中）

> 注意：墨砚v1.2.2无法在Windows 7上运行。

从其它站点下载：
+ https://github.com/XmacsLabs/mogan/releases
+ https://gitee.com/XmacsLabs/mogan/releases
+ https://codeberg.org/XmacsLabs/mogan/releases

旧版本安装包：
+ [v1.2.1 (2023/12/21)](https://github.com/XmacsLabs/mogan/releases/tag/v1.2.1)
+ [v1.2.0 (2023/12/04)](https://gitee.com/XmacsLabs/mogan/releases/tag/v1.2.0)
+ [**v1.1.6 (2023/09/29)**](https://gitee.com/XmacsLabs/mogan/releases/tag/v1.1.6)
+ [v1.1.5 (2023/08/11)](https://gitee.com/XmacsLabs/mogan/releases/tag/v1.1.5)
+ [v1.1.4 (2023/07/31)](https://gitee.com/XmacsLabs/mogan/releases/tag/v1.1.4)
+ [v1.1.3 (2023/06/05)](https://gitee.com/XmacsLabs/mogan/releases/tag/v1.1.3)
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

# 如何安装
当前最新稳定版是墨干理工套件v1.2.4，可以在`帮助 -> 欢迎`这个菜单项中检查是否有新版可用。

## 官方提供的安装包
| 产品 | 操作系统 | 马上下载 |
|-----|-------|-----|
| 墨干v1.2.4 | Windows安装包 (>=10)| [⏬ 下载](https://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/v1.2.4/MoganResearch-v1.2.4-64bit-installer.exe) |
| 墨干v1.2.4 | Windows便携版 (>=10)| [⏬ 下载](https://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/v1.2.4/MoganResearch-v1.2.4-64bit-installer.zip) |
| 墨干v1.2.4 | macOS (>=11) | [⏬ 下载](https://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/v1.2.4/MoganResearch-v1.2.4.dmg) |
| 墨干v1.2.4 | macOS M1/M2 (>=13) | [⏬ 下载](https://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/v1.2.4/MoganResearch-v1.2.4-arm.dmg) |
| 墨干v1.2.4 | Ubuntu 22.04 | [⏬ 下载](https://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/v1.2.4/mogan-research-v1.2.4-ubuntu22.04.deb) |

SHA256校验码：
```
c66ff1721e418ccfc8733119a970820675c16dba034fd1b599d928726f61f673  MoganResearch-v1.2.4-64bit-installer.exe
f4a5b46a67581b864d7cf58df4d4a3c6a6bcef28c6e7419f8729bc0529cf224d  MoganResearch-v1.2.4-64bit-installer.zip
21dabce305d35de338aa7453fcd6e146a8d3df2e6792f7617f2092c26b7bcc82  MoganResearch-v1.2.4-arm.dmg
d80bdc9ba4d1a028ca947e1d577ba1ffae3e4bae57471d020c87262b945c4166  MoganResearch-v1.2.4.dmg
9f95cc572a3b75f2f3d3757c2625477ec97c1361bba3dbcddf5dfe13371a00a3  mogan-research-v1.2.4-ubuntu22.04.deb
```

墨干理工套件：
+ 墨干（提供Windows/macOS/Ubuntu三大平台安装包）
+ 墨码（无安装包，正在开发中）
+ 墨板（无安装包，正在开发中）

> 注意：墨干v1.2.4无法在Windows 7上运行。

从其它站点下载：
+ https://github.com/XmacsLabs/mogan/releases
+ https://gitee.com/XmacsLabs/mogan/releases
+ https://codeberg.org/XmacsLabs/mogan/releases

## 推荐安装的旧版
有时候，你可能希望使用旧版，下面是一些推荐版本：
+ [**v1.1.6**](https://gitee.com/XmacsLabs/mogan/releases/tag/v1.1.6)
+ [**v1.1.3**](https://github.com/XmacsLabs/mogan/releases/tag/v1.1.3)

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

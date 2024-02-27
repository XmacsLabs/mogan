# 如何安装
当前最新稳定版是墨干理工套件 V1.2.5 LTS，可以在`帮助 -> 欢迎`这个菜单项中检查是否有新版可用。

## 官方提供的安装包
| 产品 | 操作系统 | 马上下载 |
|-----|-------|-----|
| 墨干v1.2.5 | Windows安装包 (>=10)| [⏬ 下载](https://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/v1.2.5/MoganResearch-v1.2.5-64bit-installer.exe) |
| 墨干v1.2.5 | Windows便携版 (>=10)| [⏬ 下载](https://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/v1.2.5/MoganResearch-v1.2.5-64bit-installer.zip) |
| 墨干v1.2.5 | macOS (>=11) | [⏬ 下载](https://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/v1.2.5/MoganResearch-v1.2.5.dmg) |
| 墨干v1.2.5 | macOS M1/M2 (>=13) | [⏬ 下载](https://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/v1.2.5/MoganResearch-v1.2.5-arm.dmg) |
| 墨干v1.2.5 | Ubuntu 20.04 | [⏬ 下载](https://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/v1.2.5/mogan-research-v1.2.5-ubuntu20.04.deb) |
| 墨干v1.2.5 | Ubuntu 22.04 | [⏬ 下载](https://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/v1.2.5/mogan-research-v1.2.5-ubuntu22.04.deb) |
| 墨干v1.2.5 | Debian 12 | [⏬ 下载](http://mirrors.ustc.edu.cn/github-release/xmacslabs/mogan/v1.2.5/mogan-research-v1.2.5-debian12.deb) |

SHA256校验码：
```
f4d485b2add98b8d9618b2a82ba3f6720fad78fcdefc91eb6263d3cb4c39a370  MoganResearch-v1.2.5-64bit-installer.exe
456219c8ca3db8ceac4a5a9f47385d56405f06c32a09d3ffd9acc7ba34d35ac9  MoganResearch-v1.2.5-64bit-installer.zip
8164c4e7766c72a0010d7db7abd62d338d8ede7aca22bc5550391937d7d301f5  MoganResearch-v1.2.5-arm.dmg
361354aad2a5de93d22508bb784c0e940a43d28d559dda79803499f4fb3c0dc6  MoganResearch-v1.2.5.dmg
498c232ec032d41d8cfa3edc10814ff7a0015818fe43fe8542f9807e2ec04b18  mogan-research-v1.2.5-debian12.deb
558ad6a1dd4f384d33b0dfed9a1171ebcccdb71f417173a2f0f010d838c35dbf  mogan-research-v1.2.5-ubuntu20.04.deb
a147c7630b99592f77437172de3245e11f930bff23c61c622d0a03a4b0104daa  mogan-research-v1.2.5-ubuntu22.04.deb
```

墨干理工套件：
+ 墨干（提供Windows/macOS/Ubuntu/Debian四大平台安装包）
+ 墨码（无安装包，正在开发中）
+ 墨板（无安装包，正在开发中）

> 注意：墨干v1.2.5无法在Windows 7上运行。

从其它站点下载：
+ https://github.com/XmacsLabs/mogan/releases
+ https://gitee.com/XmacsLabs/mogan/releases
+ https://codeberg.org/XmacsLabs/mogan/releases

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

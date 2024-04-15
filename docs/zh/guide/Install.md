# 如何安装
当前最新稳定版是墨干理工套件 v1.2.5.3 LTS，可以在`帮助 -> 欢迎`这个菜单项中检查是否有新版可用。

## 官方提供的安装包
| 产品 | 操作系统 | 马上下载 |
|-----|-------|-----|
| 墨干v1.2.5.3 | Windows安装包 (>=10)| [⏬ 下载](https://gitee.com/XmacsLabs/mogan/releases/download/v1.2.5.3/MoganResearch-v1.2.5.3-64bit-installer.exe) |
| 墨干v1.2.5.3 | Windows便携版 (>=10)| [⏬ 下载](https://gitee.com/XmacsLabs/mogan/releases/download/v1.2.5.3/MoganResearch-v1.2.5.3-64bit-installer.zip) |
| 墨干v1.2.5.3 | macOS (>=12) | [⏬ 下载](https://gitee.com/XmacsLabs/mogan/releases/download/v1.2.5.3/MoganResearch-v1.2.5.3.dmg) |
| 墨干v1.2.5.3 | macOS arm64 (>=13) | [⏬ 下载](https://gitee.com/XmacsLabs/mogan/releases/download/v1.2.5.3/MoganResearch-v1.2.5.3-arm.dmg) |
| 墨干v1.2.5.3 | Ubuntu 20.04 | [⏬ 下载](https://gitee.com/XmacsLabs/mogan/releases/download/v1.2.5.3/mogan-research-v1.2.5.3-ubuntu20.04.deb) |
| 墨干v1.2.5.3 | Ubuntu 22.04 | [⏬ 下载](https://gitee.com/XmacsLabs/mogan/releases/download/v1.2.5.3/mogan-research-v1.2.5.3-ubuntu22.04.deb) |
| 墨干v1.2.5.3 | Debian 12 | [⏬ 下载](https://gitee.com/XmacsLabs/mogan/releases/download/v1.2.5.3/mogan-research-v1.2.5.3-debian12.deb) |

SHA256校验码：
```
71544fa036ae941f9a5b29574f05d1120e05ea44d27ce24a9f197711741ed32a  MoganResearch-v1.2.5.3-64bit-installer.exe
741adce5925314c9daaaeaf0c60dde691fc7737080567da1757e0884fa28dc7e  MoganResearch-v1.2.5.3-64bit-installer.zip
7839ee6df89e4d4fb4ed03d60893dbbccedf0d45e3b5cbaaf1adfb7a01e86f9f  MoganResearch-v1.2.5.3-arm.dmg
eb235b4013b5cb5752d465bee54b85c48466665bd16e12652c6b025e289725ed  MoganResearch-v1.2.5.3.dmg
b5134d8bc28c2b56ab006ba90e4ef5419846fb72ff33e5daf1d294a0dcd8d308  mogan-research-v1.2.5.3-debian12.deb
0c0736eb42bfab5d80f6c1f00a958dcae75cde659f25b662cd46eeb90c4652c2  mogan-research-v1.2.5.3-ubuntu20.04.deb
4b0a9aaad332a2bb4fc670f1a8de588f79dae8c17ebae6202e257d2f6e8434bf  mogan-research-v1.2.5.3-ubuntu22.04.deb
```

墨干理工套件：
+ 墨干（提供Windows/macOS/Ubuntu/Debian四大平台安装包）
+ 墨码（无安装包，正在开发中）
+ 墨板（无安装包，正在开发中）

> 注意：墨干v1.2.5.3无法在Windows 7上运行。

从其它站点下载：
+ https://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/
+ https://github.com/XmacsLabs/mogan/releases
+ https://gitee.com/XmacsLabs/mogan/releases
+ https://codeberg.org/XmacsLabs/mogan/releases

下面是在不同的系统上安装的具体步骤：

## Debian 12
下载官方提供的deb然后：
```
sudo apt install ./mogan-research-v1.2.5.3-debian12.deb
```

## Ubuntu 20.04/Ubuntu 22.04
下载官方提供的deb然后：
```
# For ubuntu 20.04
sudo apt install ./mogan-research-v1.2.5.3-ubuntu20.04.deb

# For ubuntu 22.04
sudo apt install ./mogan-research-v1.2.5.3-ubuntu22.04.deb
```

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

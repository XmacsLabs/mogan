# 如何安装
当前最新稳定版是墨干理工套件V1.2.8，可以在`帮助 -> 欢迎`这个菜单项中检查是否有新版可用。

## 官方提供的安装包
| 产品 | 操作系统 | 马上下载 |
|-----|-------|-----|
| 墨干v1.2.8 | Windows安装包 (>=10)| [⏬ 下载](https://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/v1.2.8/MoganResearch-v1.2.8-64bit-installer.exe) |
| 墨干v1.2.8 | Windows便携版 (>=10)| [⏬ 下载](https://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/v1.2.8/MoganResearch-v1.2.8-64bit-installer.zip) |
| 墨干v1.2.8 | macOS arm64 (>=13) | [⏬ 下载](https://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/v1.2.8/MoganResearch-v1.2.8-arm.dmg) |
| 墨干v1.2.8 | macOS (>=12) | [⏬ 下载](https://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/v1.2.8/MoganResearch-v1.2.8.dmg) |
| 墨干v1.2.8 | Debian 12 | [⏬ 下载](https://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/v1.2.8/mogan-research-v1.2.8-debian12.deb) |
| 墨干v1.2.8 | Ubuntu 22.04 | [⏬ 下载](https://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/v1.2.8/mogan-research-v1.2.8-ubuntu22.04.deb) |
| 墨干v1.2.8 | Ubuntu 24.04 | [⏬ 下载](https://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/v1.2.8/mogan-research-v1.2.8-ubuntu24.04.deb) |

SHA256校验码：
```
6bd14e6f2f7f9eb12c5fe5dd42617b48dec33b879592bdd2774ee0dbcfb3745e  MoganResearch-v1.2.8-64bit-installer.exe
9c691e0d187f67d3378df27fafd8024abc75b068b2afe87a3e05ba83fb43a1b9  MoganResearch-v1.2.8-64bit-installer.zip
d2dc977ff90ca2204db66017dc47a9457487b9b9d2d664858ac0f951e963e05a  MoganResearch-v1.2.8-arm.dmg
49700360dc2ec131c676564d523361bf1e53421fb8bc1540e1e84f846a982a95  MoganResearch-v1.2.8.dmg
8f8982158c98fed2baa421f2212fa8f1cd21407e77f9cb90468928578f4fad5d  mogan-research-v1.2.8-debian12.deb
aa0f942b7d0dd1e9e3db0687bf978857cc4ade3500a0f80e84d4461632e39ba1  mogan-research-v1.2.8-ubuntu22.04.deb
b2ca3008ceaeca12d528470a8e6f1375b9c1d814ace4fb09bc4745f6dbe3194e  mogan-research-v1.2.8-ubuntu24.04.deb
```

墨干理工套件：
+ 墨干（提供Windows/macOS/Ubuntu/Debian四大平台安装包）
+ 墨码（无安装包，正在开发中）
+ 墨板（无安装包，正在开发中）

> 注意：墨干v1.2.6无法在Windows 7上运行。

从其它站点下载：
+ https://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/
+ https://github.com/XmacsLabs/mogan/releases
+ https://gitee.com/XmacsLabs/mogan/releases
+ https://codeberg.org/XmacsLabs/mogan/releases

下面是在不同的系统上安装的具体步骤：

## Debian 12
下载官方提供的deb然后：
```
sudo apt install ./mogan-research-v1.2.8-debian12.deb
```

## Ubuntu 22.04/Ubuntu 24.04
下载官方提供的deb然后：
```
# For ubuntu 22.04
sudo apt install ./mogan-research-v1.2.8-ubuntu22.04.deb

# For ubuntu 24.04
sudo apt install ./mogan-research-v1.2.8-ubuntu24.04.deb
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

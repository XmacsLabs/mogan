# 如何安装
当前最新稳定版是墨干理工套件 v1.2.5.4 LTS，可以在`帮助 -> 欢迎`这个菜单项中检查是否有新版可用。

## 官方提供的安装包
| 产品 | 操作系统 | 马上下载 |
|-----|-------|-----|
| 墨干v1.2.5.4 | Windows安装包 (>=10)| [⏬ 下载](https://gitee.com/XmacsLabs/mogan/releases/download/v1.2.5.4/MoganResearch-v1.2.5.4-64bit-installer.exe) |
| 墨干v1.2.5.4 | Windows便携版 (>=10)| [⏬ 下载](https://gitee.com/XmacsLabs/mogan/releases/download/v1.2.5.4/MoganResearch-v1.2.5.4-64bit-installer.zip) |
| 墨干v1.2.5.4 | macOS (>=12) | [⏬ 下载](https://gitee.com/XmacsLabs/mogan/releases/download/v1.2.5.4/MoganResearch-v1.2.5.4.dmg) |
| 墨干v1.2.5.4 | macOS arm64 (>=13) | [⏬ 下载](https://gitee.com/XmacsLabs/mogan/releases/download/v1.2.5.4/MoganResearch-v1.2.5.4-arm.dmg) |
| 墨干v1.2.5.4 | Ubuntu 20.04 | [⏬ 下载](https://gitee.com/XmacsLabs/mogan/releases/download/v1.2.5.4/mogan-research-v1.2.5.4-ubuntu20.04.deb) |
| 墨干v1.2.5.4 | Ubuntu 22.04 | [⏬ 下载](https://gitee.com/XmacsLabs/mogan/releases/download/v1.2.5.4/mogan-research-v1.2.5.4-ubuntu22.04.deb) |
| 墨干v1.2.5.4 | Debian 12 | [⏬ 下载](https://gitee.com/XmacsLabs/mogan/releases/download/v1.2.5.4/mogan-research-v1.2.5.4-debian12.deb) |

SHA256校验码：
```
3a77620e81a00fd3879f5c6bca1a82abe158a352a439400b751823e5527f27db  MoganResearch-v1.2.5.4-64bit-installer.exe
21f8555a3360653afc5af56908b6b24a3127a25cfec081dd3df1b013a15dda4d  MoganResearch-v1.2.5.4-64bit-installer.zip
371ccc5fc23d6f4e59915796fc02466a702a4e7626cfcfe25090757c60ba6ab7  MoganResearch-v1.2.5.4-arm.dmg
cabeb34ca31257e0361865bd6f46010682983c00d1288e67433670946c0f344d  MoganResearch-v1.2.5.4.dmg
41cc47ac5bb51f87bbc52eb6b7578adc59f590d94d8b40b5edf50ed91d262905  mogan-research-v1.2.5.4-debian12.deb
8482544117cfd5ec5cbe2d97ed55cddbda68f0443b34706129d85cce0acc4e71  mogan-research-v1.2.5.4-ubuntu20.04.deb
d9ecf0a3486bdb86781e49bda0d83b0b7b64822126964c69f39ac0c9ef5b1f3c  mogan-research-v1.2.5.4-ubuntu22.04.deb
```

墨干理工套件：
+ 墨干（提供Windows/macOS/Ubuntu/Debian四大平台安装包）
+ 墨码（无安装包，正在开发中）
+ 墨板（无安装包，正在开发中）

> 注意：墨干v1.2.5.4无法在Windows 7上运行。

从其它站点下载：
+ https://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/
+ https://github.com/XmacsLabs/mogan/releases
+ https://gitee.com/XmacsLabs/mogan/releases
+ https://codeberg.org/XmacsLabs/mogan/releases

下面是在不同的系统上安装的具体步骤：

## Debian 12
下载官方提供的deb然后：
```
sudo apt install ./mogan-research-v1.2.5.4-debian12.deb
```

## Ubuntu 20.04/Ubuntu 22.04
下载官方提供的deb然后：
```
# For ubuntu 20.04
sudo apt install ./mogan-research-v1.2.5.4-ubuntu20.04.deb

# For ubuntu 22.04
sudo apt install ./mogan-research-v1.2.5.4-ubuntu22.04.deb
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

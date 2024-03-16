# 如何安装
当前最新稳定版是墨干理工套件 v1.2.5.1 LTS，可以在`帮助 -> 欢迎`这个菜单项中检查是否有新版可用。

## 官方提供的安装包
| 产品 | 操作系统 | 马上下载 |
|-----|-------|-----|
| 墨干v1.2.5.1 | Windows安装包 (>=10)| [⏬ 下载](https://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/v1.2.5.1/MoganResearch-v1.2.5.1-64bit-installer.exe) |
| 墨干v1.2.5.1 | Windows便携版 (>=10)| [⏬ 下载](https://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/v1.2.5.1/MoganResearch-v1.2.5.1-64bit-installer.zip) |
| 墨干v1.2.5.1 | macOS (>=11) | [⏬ 下载](https://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/v1.2.5.1/MoganResearch-v1.2.5.1.dmg) |
| 墨干v1.2.5.1 | macOS M1/M2 (>=13) | [⏬ 下载](https://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/v1.2.5.1/MoganResearch-v1.2.5.1-arm.dmg) |
| 墨干v1.2.5.1 | Ubuntu 20.04 | [⏬ 下载](https://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/v1.2.5.1/mogan-research-v1.2.5.1-ubuntu20.04.deb) |
| 墨干v1.2.5.1 | Ubuntu 22.04 | [⏬ 下载](https://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/v1.2.5.1/mogan-research-v1.2.5.1-ubuntu22.04.deb) |
| 墨干v1.2.5.1 | Debian 12 | [⏬ 下载](http://mirrors.ustc.edu.cn/github-release/xmacslabs/mogan/v1.2.5.1/mogan-research-v1.2.5.1-debian12.deb) |

SHA256校验码：
```
e3713e1506c85dd4ff01b0565f70228486e95ff10ab67379cf0eb1c897c5aa3a  MoganResearch-v1.2.5.1-64bit-installer.exe
f05ef94ac913e7c11a5a3cefa604b5d56c7608b2613b6a7967d64d57a82cf476  MoganResearch-v1.2.5.1-64bit-installer.zip
3cb7d2417345889ad9c8eb66fce52b2168b4189c237a45d02a4c6e3bba19c3d3  MoganResearch-v1.2.5.1-arm.dmg
a2c78e59562924a19d9f744c25efa9d3cc07a105600f2798c40f8994096fe354  MoganResearch-v1.2.5.1.dmg
bb18f3db6de640ef79fa243b394340aaae9ba2f7147bfa48b417e46b1d77cf0a  mogan-research-v1.2.5.1-debian12.deb
b5c84b633c8642522c6ba04c2a8e91dc171e04f8bb922f751a969bd3e8f564ea  mogan-research-v1.2.5.1-ubuntu20.04.deb
a79d7899147a8c8e4cb14b2d77d87208db1ee5f408872ce3f31d9e9e0a4fbb80  mogan-research-v1.2.5.1-ubuntu22.04.deb
```

墨干理工套件：
+ 墨干（提供Windows/macOS/Ubuntu/Debian四大平台安装包）
+ 墨码（无安装包，正在开发中）
+ 墨板（无安装包，正在开发中）

> 注意：墨干v1.2.5.1无法在Windows 7上运行。

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

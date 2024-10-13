# 如何安装
当前最新稳定版是墨干理工套件V1.2.9.5，可以在`帮助 -> 欢迎`这个菜单项中检查是否有新版可用。

## 官方提供的安装包
| 产品 | 操作系统 | 马上下载 |
|-----|-------|-----|
| 墨干v1.2.9.5 | Windows安装包 (>=10)| [⏬ 下载](https://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/v1.2.9.5/MoganResearch-v1.2.9.5-64bit-installer.exe) |
| 墨干v1.2.9.5 | Windows绿色版 (>=10)| [⏬ 下载](https://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/v1.2.9.5/MoganResearch-v1.2.9.5-64bit-portable.zip) |
| 墨干v1.2.9.5 | macOS arm64 (>=13) | [⏬ 下载](https://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/v1.2.9.5/MoganResearch-v1.2.9.5-arm.dmg) |
| 墨干v1.2.9.5 | macOS (>=12) | [⏬ 下载](https://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/v1.2.9.5/MoganResearch-v1.2.9.5.dmg) |
| 墨干v1.2.9.5 | Debian 12 | [⏬ 下载](https://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/v1.2.9.5/mogan-research-v1.2.9.5-debian12.deb) |
| 墨干v1.2.9.5 | Ubuntu 22.04 | [⏬ 下载](https://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/v1.2.9.5/mogan-research-v1.2.9.5-ubuntu22.04.deb) |
| 墨干v1.2.9.5 | Ubuntu 24.04 | [⏬ 下载](https://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/v1.2.9.5/mogan-research-v1.2.9.5-ubuntu24.04.deb) |

SHA256校验码：
```
26076445719ae6257153b7c1a7f08f28fe648168ba8ece77f2812768d6de0fe4  MoganResearch-v1.2.9.5-64bit-installer.exe
4fa8de7ce8a7b6b4294f208d3d79e9a8b854f278796312bc0fa926237c4f8f95  MoganResearch-v1.2.9.5-64bit-portable.zip
0f87be214fa4ec030585b2068c9f8066b2bb8e753c6539b37c52c6e73fcf13c9  MoganResearch-v1.2.9.5-arm.dmg
08266def7a60ee75a6b4e0b0a21e8a2bfc3d74ed1a20f2c4a8e3b53a3b6c7c10  MoganResearch-v1.2.9.5.dmg
e81f9dbeeb816e56c0ddb61b36757737b382d50ebc34dfe11b2042ca1eff91c7  mogan-research-v1.2.9.5-debian12.deb
0d4ee6923dc9e527aabc12440a858d5fcce4c28d9b82c8047ab09f2e1f5b57e4  mogan-research-v1.2.9.5-ubuntu22.04.deb
a97f622ec0b48b81e35681eecbf0619d43481f0f59a050ee6e8fa0fbc982fac5  mogan-research-v1.2.9.5-ubuntu24.04.deb
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
sudo apt install ./mogan-research-v1.2.9.5-debian12.deb
```

## Ubuntu 22.04/Ubuntu 24.04
下载官方提供的deb然后：
```
# For ubuntu 22.04
sudo apt install ./mogan-research-v1.2.9.5-ubuntu22.04.deb

# For ubuntu 24.04
sudo apt install ./mogan-research-v1.2.9.5-ubuntu24.04.deb
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

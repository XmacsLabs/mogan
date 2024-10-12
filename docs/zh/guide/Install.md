# 如何安装
当前最新稳定版是墨干理工套件V1.2.9.2，可以在`帮助 -> 欢迎`这个菜单项中检查是否有新版可用。

## 官方提供的安装包
| 产品 | 操作系统 | 马上下载 |
|-----|-------|-----|
| 墨干v1.2.9.2 | Windows安装包 (>=10)| [⏬ 下载](https://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/v1.2.9.2/MoganResearch-v1.2.9.2-64bit-installer.exe) |
| 墨干v1.2.9.2 | Windows绿色版 (>=10)| [⏬ 下载](https://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/v1.2.9.2/MoganResearch-v1.2.9.2-64bit-portable.zip) |
| 墨干v1.2.9.2 | macOS arm64 (>=13) | [⏬ 下载](https://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/v1.2.9.2/MoganResearch-v1.2.9.2-arm.dmg) |
| 墨干v1.2.9.2 | macOS (>=12) | [⏬ 下载](https://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/v1.2.9.2/MoganResearch-v1.2.9.2.dmg) |
| 墨干v1.2.9.2 | Debian 12 | [⏬ 下载](https://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/v1.2.9.2/mogan-research-v1.2.9.2-debian12.deb) |
| 墨干v1.2.9.2 | Ubuntu 22.04 | [⏬ 下载](https://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/v1.2.9.2/mogan-research-v1.2.9.2-ubuntu22.04.deb) |
| 墨干v1.2.9.2 | Ubuntu 24.04 | [⏬ 下载](https://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/v1.2.9.2/mogan-research-v1.2.9.2-ubuntu24.04.deb) |

SHA256校验码：
```
700eeb39fa4f5eaba4ee8839f6a813488a8f5f758722f4fb0e1c5c0c7240a70e  MoganResearch-v1.2.9.2-64bit-installer.exe
469f1e48e2a780345bf359157b27ef43097f3883b7f27e58041a144cc0cc990b  MoganResearch-v1.2.9.2-64bit-portable.zip
cbca96dfc2f1073762cebf56c642964b37802b5bbe9e6bac3475cfe6962e6968  MoganResearch-v1.2.9.2-arm.dmg
aa429c0e656ccd2309ce0bdc1e3087a605052c66c205ec3bf37f5fb812aed19e  MoganResearch-v1.2.9.2.dmg
9266c44775e0d3539adfb9816c45a326cbb74da27ea267bba8989234b5907566  mogan-research-v1.2.9.2-debian12.deb
04878ac96b25af8ad2ab9c2551713cef7d9e2e650460f0f2188b6f103faeb611  mogan-research-v1.2.9.2-ubuntu22.04.deb
aab31ce801d4d9dca9abe4c4ac4f5eddcc18e16934c6e16a80831562a520b1cb  mogan-research-v1.2.9.2-ubuntu24.04.deb
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
sudo apt install ./mogan-research-v1.2.9.2-debian12.deb
```

## Ubuntu 22.04/Ubuntu 24.04
下载官方提供的deb然后：
```
# For ubuntu 22.04
sudo apt install ./mogan-research-v1.2.9.2-ubuntu22.04.deb

# For ubuntu 24.04
sudo apt install ./mogan-research-v1.2.9.2-ubuntu24.04.deb
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

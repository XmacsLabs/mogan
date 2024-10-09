# 如何安装
当前最新稳定版是墨干理工套件V1.2.9.3，可以在`帮助 -> 欢迎`这个菜单项中检查是否有新版可用。

## 官方提供的安装包
| 产品 | 操作系统 | 马上下载 |
|-----|-------|-----|
| 墨干v1.2.9.3 | Windows安装包 (>=10)| [⏬ 下载](https://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/v1.2.9.3/MoganResearch-v1.2.9.3-64bit-installer.exe) |
| 墨干v1.2.9.3 | Windows绿色版 (>=10)| [⏬ 下载](https://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/v1.2.9.3/MoganResearch-v1.2.9.3-64bit-portable.zip) |
| 墨干v1.2.9.3 | macOS arm64 (>=13) | [⏬ 下载](https://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/v1.2.9.3/MoganResearch-v1.2.9.3-arm.dmg) |
| 墨干v1.2.9.3 | macOS (>=12) | [⏬ 下载](https://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/v1.2.9.3/MoganResearch-v1.2.9.3.dmg) |
| 墨干v1.2.9.3 | Debian 12 | [⏬ 下载](https://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/v1.2.9.3/mogan-research-v1.2.9.3-debian12.deb) |
| 墨干v1.2.9.3 | Ubuntu 22.04 | [⏬ 下载](https://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/v1.2.9.3/mogan-research-v1.2.9.3-ubuntu22.04.deb) |
| 墨干v1.2.9.3 | Ubuntu 24.04 | [⏬ 下载](https://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/v1.2.9.3/mogan-research-v1.2.9.3-ubuntu24.04.deb) |

SHA256校验码：
```
ca0662525915a4d3983c4891d984526f5225fd63352b44f60db7b679f293becc  MoganResearch-v1.2.9.3-64bit-installer.exe
3b51f7bbb3e90c6a542010700095d7517db61d25f5e6f1b3d7adf8b44345c624  MoganResearch-v1.2.9.3-64bit-portable.zip
74cb464ddde67d8c717c87362d5ad4b29c095be19de85ee9363f6b459ad05049  MoganResearch-v1.2.9.3-arm.dmg
25bc5fc683bcc60ee720a5995b8ad5990fd702365baea5e7d0590973675cf746  MoganResearch-v1.2.9.3.dmg
ac21d63d4e1f550b3485939030658ac2980014240d9b7b77a080013b762f9b91  mogan-research-v1.2.9.3-debian12.deb
6f1fc4342471e3e1493976e56a30becc59c394569baa603cd2054005c02106a3  mogan-research-v1.2.9.3-ubuntu22.04.deb
a287a607caaa188b0b5ba96cedf333c1da9d8b82261c163861ea8e4ef5f7f081  mogan-research-v1.2.9.3-ubuntu24.04.deb
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
sudo apt install ./mogan-research-v1.2.9.3-debian12.deb
```

## Ubuntu 22.04/Ubuntu 24.04
下载官方提供的deb然后：
```
# For ubuntu 22.04
sudo apt install ./mogan-research-v1.2.9.3-ubuntu22.04.deb

# For ubuntu 24.04
sudo apt install ./mogan-research-v1.2.9.3-ubuntu24.04.deb
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

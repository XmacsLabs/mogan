# 如何安装
当前最新稳定版是墨干理工套件V1.2.9.7，可以在`帮助 -> 欢迎`这个菜单项中检查是否有新版可用。

## 官方提供的安装包
| 产品 | 操作系统 | 马上下载 |
|-----|-------|-----|
| 墨干v1.2.9.7 | Windows安装包 (>=10)| [⏬ 下载](https://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/v1.2.9.7/MoganResearch-v1.2.9.7-64bit-installer.exe) |
| 墨干v1.2.9.7 | Windows绿色版 (>=10)| [⏬ 下载](https://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/v1.2.9.7/MoganResearch-v1.2.9.7-64bit-portable.zip) |
| 墨干v1.2.9.7 | macOS arm64 (>=13) | [⏬ 下载](https://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/v1.2.9.7/MoganResearch-v1.2.9.7-arm.dmg) |
| 墨干v1.2.9.7 | macOS (>=13) | [⏬ 下载](https://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/v1.2.9.7/MoganResearch-v1.2.9.7.dmg) |
| 墨干v1.2.9.7 | Ubuntu 22.04 | [⏬ 下载](https://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/v1.2.9.7/mogan-research-v1.2.9.7-ubuntu22.04.deb) |
| 墨干v1.2.9.7 | Ubuntu 24.04 | [⏬ 下载](https://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/v1.2.9.7/mogan-research-v1.2.9.7-ubuntu24.04.deb) |
| 墨干v1.2.9.7 | Fedora 41 | [⏬ 下载](https://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/v1.2.9.7/mogan-research-v1.2.9.7-fedora41.rpm) |
| 墨干v1.2.9.7 | Debian 12 | [⏬ 下载](https://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/v1.2.9.7/mogan-research-v1.2.9.7-debian12.deb) |

SHA256校验码：
```
3eeb3f15b930110b22febe3a27e937efaceaa7821d62c6b73a1f17484825dc46  MoganResearch-v1.2.9.7-64bit-installer.exe
c51a9c0f801f23e1ba886f7133277aefc213932b8495abb17168a6ff4604dc36  MoganResearch-v1.2.9.7-64bit-portable.zip
4b2dbfe74492031f12760701c5f878b98bb1fb2abc2e449f7d692ba2dc4b9939  MoganResearch-v1.2.9.7-arm.dmg
a31276e0091b008e3ff9a2f3f1a6e00828221371fe073cb53d2760d349af4fa0  MoganResearch-v1.2.9.7.dmg
810a0d341bab1b58f05e73f759416aee944f72821c9121884bf6df0f65bd51cf  mogan-research-v1.2.9.7-debian12.deb
fc6171d44e29351c2bd16fffbbb18ed287ef20b8871f7b9ca2ce898587dd948c  mogan-research-v1.2.9.7-fedora41.rpm
f4bcb8d4a9bec63b158c93fc0efbc083da95de84ddf7fb84ecf56675120458e5  mogan-research-v1.2.9.7-ubuntu22.04.deb
6716064ec9f570aad3cbd8a84251d06cbbc7055f4875b323d2c7718a39ac9231  mogan-research-v1.2.9.7-ubuntu24.04.deb
```

墨干理工套件：
+ 墨干（提供Windows/macOS/Ubuntu/Debian四大平台安装包）
+ 墨码（无安装包，正在开发中）
+ 墨板（无安装包，正在开发中）

> 注意：墨干（>= v1.2.6）无法在Windows 7上运行。

从其它站点下载：
+ https://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/
+ https://github.com/XmacsLabs/mogan/releases
+ https://gitee.com/XmacsLabs/mogan/releases
+ https://codeberg.org/XmacsLabs/mogan/releases

下面是在不同的系统上安装的具体步骤：

## Debian 12
下载官方提供的deb然后：
```
sudo apt install ./mogan-research-v1.2.9.7-debian12.deb
```

## Ubuntu 22.04/Ubuntu 24.04
下载官方提供的deb然后：
```
# For ubuntu 22.04
sudo apt install ./mogan-research-v1.2.9.7-ubuntu22.04.deb

# For ubuntu 24.04
sudo apt install ./mogan-research-v1.2.9.7-ubuntu24.04.deb
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

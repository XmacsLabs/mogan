# 如何安装
当前最新稳定版是墨干理工套件V1.2.9，可以在`帮助 -> 欢迎`这个菜单项中检查是否有新版可用。

## 官方提供的安装包
| 产品 | 操作系统 | 马上下载 |
|-----|-------|-----|
| 墨干v1.2.9 | Windows安装包 (>=10)| [⏬ 下载](https://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/v1.2.9/MoganResearch-v1.2.9-64bit-installer.exe) |
| 墨干v1.2.9 | Windows绿色版 (>=10)| [⏬ 下载](https://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/v1.2.9/MoganResearch-v1.2.9-64bit-portable.zip) |
| 墨干v1.2.9 | macOS arm64 (>=13) | [⏬ 下载](https://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/v1.2.9/MoganResearch-v1.2.9-arm.dmg) |
| 墨干v1.2.9 | macOS (>=12) | [⏬ 下载](https://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/v1.2.9/MoganResearch-v1.2.9.dmg) |
| 墨干v1.2.9 | Debian 12 | [⏬ 下载](https://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/v1.2.9/mogan-research-v1.2.9-debian12.deb) |
| 墨干v1.2.9 | Ubuntu 22.04 | [⏬ 下载](https://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/v1.2.9/mogan-research-v1.2.9-ubuntu22.04.deb) |
| 墨干v1.2.9 | Ubuntu 24.04 | [⏬ 下载](https://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/v1.2.9/mogan-research-v1.2.9-ubuntu24.04.deb) |

SHA256校验码：
```
ac4132447861da8e0cc94646900db11c5f2ead8d68b3b67a7517e49a32ecf4e7  MoganResearch-v1.2.9-64bit-installer.exe
16ce3e3ea3dd7f17c3a9c621d0ff40b281240d81b1f88a1026159d52bc54622d  MoganResearch-v1.2.9-64bit-portable.zip
bc3794c246549d67a6ee0d07b6f4c95a3685c7b6647e842a82876f31d66dd2ca  MoganResearch-v1.2.9-arm.dmg
f12cb7d30f120a8a796b4a666068dd93c30a8c290850beb393ffd21300c4ad21  MoganResearch-v1.2.9.dmg
30db47dfde5122fd72fd0fcee3bf5eaf0138fc8ed607a12e3d1d26122c4529f1  mogan-research-v1.2.9-debian12.deb
09e0a6e2eaad4077109792ebcb14d9cbd67a656aa11f550263cb45c3c0cc86e4  mogan-research-v1.2.9-ubuntu22.04.deb
f720337333f025fa31de1ffc60ad8e488049149d3fa00868b6a7ffcd0015db8b  mogan-research-v1.2.9-ubuntu24.04.deb
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
sudo apt install ./mogan-research-v1.2.9-debian12.deb
```

## Ubuntu 22.04/Ubuntu 24.04
下载官方提供的deb然后：
```
# For ubuntu 22.04
sudo apt install ./mogan-research-v1.2.9-ubuntu22.04.deb

# For ubuntu 24.04
sudo apt install ./mogan-research-v1.2.9-ubuntu24.04.deb
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

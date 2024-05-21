# 如何安装
当前最新稳定版是墨干理工套件V1.2.6，可以在`帮助 -> 欢迎`这个菜单项中检查是否有新版可用。

## 官方提供的安装包
| 产品 | 操作系统 | 马上下载 |
|-----|-------|-----|
| 墨干v1.2.6 | Windows安装包 (>=10)| [⏬ 下载](https://gitee.com/XmacsLabs/mogan/releases/download/v1.2.6/MoganResearch-v1.2.6-64bit-installer.exe) |
| 墨干v1.2.6 | Windows便携版 (>=10)| [⏬ 下载](https://gitee.com/XmacsLabs/mogan/releases/download/v1.2.6/MoganResearch-v1.2.6-64bit-installer.zip) |
| 墨干v1.2.6 | macOS (>=12) | [⏬ 下载](https://gitee.com/XmacsLabs/mogan/releases/download/v1.2.6/MoganResearch-v1.2.6.dmg) |
| 墨干v1.2.6 | macOS arm64 (>=13) | [⏬ 下载](https://gitee.com/XmacsLabs/mogan/releases/download/v1.2.6/MoganResearch-v1.2.6-arm.dmg) |
| 墨干v1.2.6 | Ubuntu 20.04 | [⏬ 下载](https://gitee.com/XmacsLabs/mogan/releases/download/v1.2.6/mogan-research-v1.2.6-ubuntu20.04.deb) |
| 墨干v1.2.6 | Ubuntu 22.04 | [⏬ 下载](https://gitee.com/XmacsLabs/mogan/releases/download/v1.2.6/mogan-research-v1.2.6-ubuntu22.04.deb) |
| 墨干v1.2.6 | Debian 12 | [⏬ 下载](https://gitee.com/XmacsLabs/mogan/releases/download/v1.2.6/mogan-research-v1.2.6-debian12.deb) |

SHA256校验码：
```
13470c2e8484e5dfdc7d2dedc10e30413c8561ba9011ce41896c0cf3966b4652  MoganResearch-v1.2.6-64bit-installer.exe
27265db9fb2523dad86ffc423bee643d46378374b8240dd7c483e80c1ef6de04  MoganResearch-v1.2.6-64bit-installer.zip
a34f8679117d1a18660d10f36fa44d9d86709ab53f57a4cfb73d4b4a0c00cecb  MoganResearch-v1.2.6-arm.dmg
9d2efc2521235bc5166a086c59e803fac4b13d096b9f0449d39d00edfa46d470  MoganResearch-v1.2.6.dmg
7f6abaa4f81bf610ca437c5762845bb0331aed92d07133c7c4025e435931d602  mogan-research-v1.2.6-debian12.deb
3f94a0e7ef8d99b767d58e7cba6dae2593797271a4391a837cf22ee205d45ade  mogan-research-v1.2.6-ubuntu22.04.deb
cfd3ce32c1bdb9efb3f34a5a06fa567dd5624c93357df04290f06bcd7c5d179b  mogan-research-v1.2.6-ubuntu24.04.deb
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
sudo apt install ./mogan-research-v1.2.6-debian12.deb
```

## Ubuntu 20.04/Ubuntu 22.04
下载官方提供的deb然后：
```
# For ubuntu 20.04
sudo apt install ./mogan-research-v1.2.6-ubuntu20.04.deb

# For ubuntu 22.04
sudo apt install ./mogan-research-v1.2.6-ubuntu22.04.deb
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

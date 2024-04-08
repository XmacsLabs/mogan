# 如何安装
当前最新稳定版是墨干理工套件 v1.2.5.1 LTS，可以在`帮助 -> 欢迎`这个菜单项中检查是否有新版可用。

## 官方提供的安装包
| 产品 | 操作系统 | 马上下载 |
|-----|-------|-----|
| 墨干v1.2.5.2 | Windows安装包 (>=10)| [⏬ 下载](https://gitee.com/XmacsLabs/mogan/releases/download/v1.2.5.2/MoganResearch-v1.2.5.2-64bit-installer.exe) |
| 墨干v1.2.5.2 | Windows便携版 (>=10)| [⏬ 下载](https://gitee.com/XmacsLabs/mogan/releases/download/v1.2.5.2/MoganResearch-v1.2.5.2-64bit-installer.zip) |
| 墨干v1.2.5.2 | macOS (>=12) | [⏬ 下载](https://gitee.com/XmacsLabs/mogan/releases/download/v1.2.5.2/MoganResearch-v1.2.5.2.dmg) |
| 墨干v1.2.5.2 | macOS arm64 (>=13) | [⏬ 下载](https://gitee.com/XmacsLabs/mogan/releases/download/v1.2.5.2/MoganResearch-v1.2.5.2-arm.dmg) |
| 墨干v1.2.5.2 | Ubuntu 20.04 | [⏬ 下载](https://gitee.com/XmacsLabs/mogan/releases/download/v1.2.5.2/mogan-research-v1.2.5.2-ubuntu20.04.deb) |
| 墨干v1.2.5.2 | Ubuntu 22.04 | [⏬ 下载](https://gitee.com/XmacsLabs/mogan/releases/download/v1.2.5.2/mogan-research-v1.2.5.2-ubuntu22.04.deb) |
| 墨干v1.2.5.2 | Debian 12 | [⏬ 下载](https://gitee.com/XmacsLabs/mogan/releases/download/v1.2.5.2/mogan-research-v1.2.5.2-debian12.deb) |

SHA256校验码：
```
3329ad096911dc89374d3648d8243efff9b4242cbd333678b89ebf5e2272887b  MoganResearch-v1.2.5.2-64bit-installer.exe
d7808e16931ae2e00b48ee5ef5987dffbd6a64956a691b84988c930a52066a00  MoganResearch-v1.2.5.2-64bit-installer.zip
25f7241a5e535be29afa0af66bbc1e737ba684fc87518902d7755d4633fd253b  MoganResearch-v1.2.5.2-arm.dmg
eec89670744b1d68e73ef0ca0ca731ca1ecabd99b4dc9d574cdc81b018c443ec  MoganResearch-v1.2.5.2.dmg
2456b8998757528d34bb2cb8998463eb2f7a64ee9cfdd47552b9358c888b98a2  mogan-research-v1.2.5.2-debian12.deb
cb66f18e9915489ec63025fcbc3a275e72f0337ddf15204e3859c34462b74301  mogan-research-v1.2.5.2-ubuntu20.04.deb
62081650da6281b3806416c0493bbb6ef5c9755a1b4f3c3df3101c6d9ca7b582  mogan-research-v1.2.5.2-ubuntu22.04.deb
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

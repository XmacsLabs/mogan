# 如何安装
当前最新稳定版是墨干理工套件V1.2.1，可以在`帮助 -> 欢迎`中检查是否有新版可用。

## V1.2.1的已知问题
+ 使用结构化的搜索或者替换窗口时容易崩溃
  + 解决方案：使用默认的底部搜索/替换工具栏
+ 切换到`编辑->首选项->通用->缓冲区管理->Documents in separate windows`容易崩溃
  + 解决方案：使用默认的`Multiple documents share window`缓冲区管理选项

墨干理工套件V1.2.2将于2024/01/31前发布。

## 官方提供的安装包
| 产品 | 操作系统 | 马上下载 | MD5校验 |
|-----|-------|-----|--------|
| 墨砚V1.2.1 | Windows (>=10)| [⏬ 下载](https://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/v1.2.1/MoganResearch-v1.2.1-64bit-installer.exe) | 619998f9b78541520513f1d9d620c919 |
| 墨砚V1.2.1 | macOS (>=11) | [⏬ 下载](https://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/v1.2.1/MoganResearch-v1.2.1.dmg) | f641259b963a4cc5b754e6972b77bddb |
| 墨砚V1.2.1 | macOS M1/M2 (>=13) | [⏬ 下载](https://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/v1.2.1/MoganResearch-v1.2.1-arm.dmg) | 702f13e33157635d5527be6bd112c3ab |
| 墨砚V1.2.1 | Ubuntu 22.04 | [⏬ 下载](https://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/v1.2.1/mogan-research-v1.2.1-ubuntu22.04.deb) | fb3ac4c59f2eb257adcefefa4340f933 |
| 墨砚V1.2.1 | UOS | [⏬ 下载](https://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/v1.2.1/mogan-research-v1.2.1-uos.deb) | 15126b1a83c4967fea2adbb2bcb7182d |
| 墨砚V1.2.1 | UOS Loongarch | [⏬ 下载](https://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/v1.2.1/mogan-research-v1.2.1-uos-loongarch64.deb) | 8d40fcddd670f8f976f9ca3d7b95526b |

墨干理工套件:
+ 墨砚（提供Windows/macOS/Ubuntu/UOS四大平台安装包）
+ 墨码（无安装包，正在开发中）
+ 墨板（无安装包，正在开发中）

> 注意：墨砚V1.2.1无法在Windows 7上运行。

从其它站点下载：
+ https://github.com/XmacsLabs/mogan/releases
+ https://gitee.com/XmacsLabs/mogan/releases
+ https://codeberg.org/XmacsLabs/mogan/releases

旧版本安装包：
+ [v1.2.0 (2023/12/04)](https://gitee.com/XmacsLabs/mogan/releases/tag/v1.2.0)
+ [v1.1.6 (2023/09/29)](https://gitee.com/XmacsLabs/mogan/releases/tag/v1.1.6)
+ [v1.1.5 (2023/08/11)](https://gitee.com/XmacsLabs/mogan/releases/tag/v1.1.5)
+ [v1.1.4 (2023/07/31)](https://gitee.com/XmacsLabs/mogan/releases/tag/v1.1.4)
+ [v1.1.3 (2023/06/05)](https://gitee.com/XmacsLabs/mogan/releases/tag/v1.1.3)
+ [v1.1.2 (2023/04/09)](https://gitee.com/XmacsLabs/mogan/releases/tag/v1.1.2)
+ [v1.1.1 (2022/10/29)](https://gitee.com/XmacsLabs/mogan/releases/tag/v1.1.1)
+ [v1.1.0 (2022/08/31)](https://gitee.com/XmacsLabs/mogan/releases/tag/v1.1.0)


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

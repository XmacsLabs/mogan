# 如何安装
## 官方提供的安装包
| 操作系统 | 马上下载 | MD5校验 |
|-------|-----|--------|
| Windows (>=10)| [Gitee](https://gitee.com/XmacsLabs/mogan/attach_files/1370402/download)/[Github](https://github.com/XmacsLabs/mogan/releases/download/v1.1.2/Mogan-v1.1.2-64bit-installer.exe) | a3ad8bccb60e161bb733b125949eeadd |
| macOS (>=10.15) | [Gitee](https://gitee.com/XmacsLabs/mogan/attach_files/1370400/download)/[Github](https://github.com/XmacsLabs/mogan/releases/download/v1.1.2/Mogan_v1.1.2.dmg) | 70a5e1107445fed61543c01bce053224 |
| macOS arm (>=12.6) | [Gitee](https://gitee.com/XmacsLabs/mogan/attach_files/1370409/download)/[Github](https://github.com/XmacsLabs/mogan/releases/download/v1.1.2/Mogan_arm_v1.1.2.dmg) | b5d56e8faef9d94867624c7f0f196c32 |
| Ubuntu 20.04 | [Gitee](https://gitee.com/XmacsLabs/mogan/attach_files/1370401/download)/[Github](https://github.com/XmacsLabs/mogan/releases/download/v1.1.2/mogan-v1.1.2-ubuntu20.04.deb) | 1c8e3664deb0606766310ec323b3777e |
| Ubuntu 22.04 | [Gitee](https://gitee.com/XmacsLabs/mogan/attach_files/1370403/download)/[Github](https://github.com/XmacsLabs/mogan/releases/download/v1.1.2/mogan-v1.1.2-ubuntu22.04.deb) | acadc8feec924238a4faafbdac9f5f51 |

旧版本安装包：
+ [v1.1.1 (2022/10/29)](https://gitee.com/XmacsLabs/mogan/releases/tag/v1.1.1)
+ [v1.1.0 (2022/08/31)](https://gitee.com/XmacsLabs/mogan/releases/tag/v1.1.0)


## 社区提供的安装包
### GNU Guix
```
guix install mogan
```
更多信息：https://packages.guix.gnu.org/packages/mogan/

> 注意：可能会遇到中文菜单无法正常显示的问题，这并不是墨干编辑器本身的问题，您可能需要花一些时间学习如何配置Guix的中文支持。

### Windows
```
winget install -e --id XmacsLabs.Mogan
```
关于winget的更多信息: https://learn.microsoft.com/zh-cn/windows/package-manager/winget/

> 注意，winget会从Github下载安装包，如果网络有问题，还是建议手动下载安装。

### Arch Linux (AUR)
```bash
yay -S mogan
```
更多信息：https://aur.archlinux.org/packages/mogan

### openSUSE (OBS)

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

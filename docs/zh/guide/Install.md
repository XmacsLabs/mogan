# 如何安装
## 官方提供的安装包
| 操作系统 | 马上下载 | MD5校验 |
|-------|-----|--------|
| Windows (>=10)| [⏬ 下载](http://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/Mogan%20Editor%20v1.1.3/Mogan-v1.1.3-64bit-installer.exe) | f6d46f104156f83f72e9e93122b17797 |
| macOS (>=11) | [⏬ 下载](http://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/Mogan%20Editor%20v1.1.3/Mogan_v1.1.3.dmg) | 51ab76ba5ade77d604b6956131f4d489 |
| macOS arm (>=12.6) | [⏬ 下载](http://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/Mogan%20Editor%20v1.1.3/Mogan_arm_v1.1.3.dmg) | af6a5ee2b9237891feaf698148a7bca0 |
| Ubuntu 20.04 | [⏬ 下载](http://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/Mogan%20Editor%20v1.1.3/mogan-v1.1.3-ubuntu20.04.deb) | 1aa101ff1c5ba74212a5d81d0c647a2e |
| Ubuntu 22.04 | [⏬ 下载](http://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/Mogan%20Editor%20v1.1.3/mogan-v1.1.3-ubuntu22.04.deb) | 6e29e788f24b57cb742c5b0ec1cef0c8 |
| UOS amd64 | [⏬ 下载](http://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/Mogan%20Editor%20v1.1.3/mogan_1.1.3-uos-amd64.deb) | ea09c29cfbc3d81ebb9156855e89fa1c |
| UOS loongarch | [⏬ 下载](mogan_1.1.3-uos-loongarch64.deb) | 8837ce391de7558cfb387db74ff9edb2 |

从其它站点下载：
+ https://codeberg.org/XmacsLabs/mogan/releases
+ https://gitee.com/XmacsLabs/mogan/releases
+ https://github.com/XmacsLabs/mogan/releases

旧版本安装包：
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

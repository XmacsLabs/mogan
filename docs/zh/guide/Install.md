# 如何安装
当前最新稳定版是墨干理工套件V1.2.0。

## V1.2.0的已知问题
+ 问题：Windows安装之后没有在桌面创建快捷方式
  - 解决方案: 找到`C:/Program Files/MoganResearch/bin/MGResearch.exe`然后启动Mogan Research或者手动创建桌面快捷方式
+ 问题：点击`编辑->快捷键->编辑键盘快捷键`直接崩溃
  - 解决方案: 等待v1.2.1新版发布并且不要去点击这个菜单项
+ 问题：`文件->导出->LaTeX`直接崩溃
  - 解决方案: 等待v1.2.1新版发布或者使用`编辑->复制到->LaTeX`作为替代
+ 问题：初次安装时，打开Mogan Research v1.2.0太慢
  - 解决方案: 等待v1.2.1新版发布并且耐心一些

墨干理工套件V1.2.1将于2024/01/01前发布。

## 官方提供的安装包
| 产品 | 操作系统 | 马上下载 | MD5校验 |
|-----|-------|-----|--------|
| 墨砚V1.2.0 | Windows (>=10)| [⏬ 下载](https://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/v1.2.0/MoganResearch-v1.2.0-64bit-installer.exe) | f34dd47aecf9a47fad7c4b27529ef8b4 |
| 墨砚V1.2.0 | macOS (>=11) | [⏬ 下载](https://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/v1.2.0/MoganResearch-v1.2.0.dmg) | cb0e449ae95832c62dac7a16e4fecb9e |
| 墨砚V1.2.0 | macOS arm (>=13) | [⏬ 下载](https://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/v1.2.0/MoganResearch-v1.2.0-arm.dmg) | 0875c9f252ddacacc4b82df0f341f157 |
| 墨砚V1.2.0 | Ubuntu 22.04 | [⏬ 下载](https://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/v1.2.0/mogan-research-v1.2.0-ubuntu22.04.deb) | c3357688696d060ab451de27bf9364bf |

墨干理工套件:
+ 墨砚（提供Windows/macOS/Ubuntu三大平台安装包）
+ 墨码（无安装包，正在开发中）
+ 墨板（无安装包，正在开发中）

> 注意：墨砚V1.2.0无法在Windows 7上运行。

从其它站点下载：
+ https://github.com/XmacsLabs/mogan/releases
+ https://gitee.com/XmacsLabs/mogan/releases
+ https://codeberg.org/XmacsLabs/mogan/releases

旧版本安装包：
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

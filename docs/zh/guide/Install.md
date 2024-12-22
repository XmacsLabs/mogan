# 如何安装
当前最新稳定版是墨干理工套件V1.2.9.7，可以在`帮助 -> 欢迎`这个菜单项中检查是否有新版可用。

## 下载墨干

<div class="button-container">
  <DownloadButtonWindows />
  <DownloadButtonMacOS />
  <DownloadButtonUbuntu />
</div>

<style>
.button-container {
  display: flex;
  justify-content: center;
  gap: 20px;
}
</style>

SHA256校验码：
<SHA256Button />

墨干理工套件：
+ 墨干（提供Windows/macOS/Ubuntu/Debian四大平台安装包）
+ 墨码（无安装包，正在开发中）
+ 墨板（无安装包，正在开发中）

> 注意：墨干需要在Windows10和macOS13以上系统运行。

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

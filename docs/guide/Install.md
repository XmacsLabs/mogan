# Installation Guide
## Official Packages

| Operating System | Download | MD5 Checksum|
|-------|-----|--------|
| Windows (>=10)| [⏬ Download](https://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/v1.1.6/Mogan-v1.1.6-64bit-installer.exe) | 2ea3b981eca36397db3222e8485e52e7 |
| macOS (>=11) | [⏬ Download](https://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/v1.1.6/Mogan_v1.1.6.dmg) | 89ad25b821e9d0f754d569b9bfcf5077 |
| macOS arm (>=12.6) | [⏬ Download](https://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/v1.1.6/Mogan_arm_v1.1.6.dmg) | b7d44f341bd202fa251125e23d5d0a64 |
| Ubuntu 20.04 | [⏬ Download](https://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/v1.1.6/mogan-v1.1.6-ubuntu20.04.deb) | b07f95c2e083ee658a6176c5bc6dd189 |
| Ubuntu 22.04 | [⏬ Download](https://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/v1.1.6/mogan-v1.1.6-ubuntu22.04.deb) | fe177fd88e699e4700f53f63510b7c3f |
| UOS amd64 | [⏬ Download](https://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/v1.1.6/mogan-v1.1.6-uos.deb) | 64f6b3bf64082ed65014b277d4b3d481 |
| UOS loongarch | [⏬ Download](https://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/v1.1.6/mogan-v1.1.6-uos-loongarch64.deb) | 29bfc986f9291d7e478e330c5d73a66d |

Other sites to download：
+ https://codeberg.org/XmacsLabs/mogan/releases
+ https://gitee.com/XmacsLabs/mogan/releases
+ https://github.com/XmacsLabs/mogan/releases

Old releases:
+ [v1.1.5 (2023/08/11)](https://github.com/XmacsLabs/mogan/releases/tag/v1.1.5)
+ [v1.1.4 (2023/07/31)](https://github.com/XmacsLabs/mogan/releases/tag/v1.1.4)
+ [v1.1.3 (2023/06/05)](https://github.com/XmacsLabs/mogan/releases/tag/v1.1.3)
+ [v1.1.2 (2023/04/09)](https://github.com/XmacsLabs/mogan/releases/tag/v1.1.2)
+ [v1.1.1 (2022/10/29)](https://github.com/XmacsLabs/mogan/releases/tag/v1.1.1)
+ [v1.1.0 (2022/08/31)](https://github.com/XmacsLabs/mogan/releases/tag/v1.1.0)

## GNU Guix
```
guix install mogan
```
More info：https://packages.guix.gnu.org/packages/mogan/

> Note: CJK characters in the menu might be displayed improperly, please learn how to configure a Guix OS.

## Windows
```
winget install -e --id XmacsLabs.Mogan
```
More info on winget: https://learn.microsoft.com/en-us/windows/package-manager/winget/

## Arch Linux (AUR)
```bash
yay -S mogan
```
More info：https://aur.archlinux.org/packages/mogan

## openSUSE (OBS)

Use the default package management tool `zypper`:

```bash
zypper addrepo https://download.opensuse.org/repositories/home:iphelf/openSUSE_Tumbleweed/home:iphelf.repo
zypper refresh
zypper install Mogan
```

Or, use the OBS package installer [`opi`](https://software.opensuse.org/package/opi):

```bash
opi Mogan
```

More info：https://software.opensuse.org/download/package?package=Mogan&project=home%3Aiphelf

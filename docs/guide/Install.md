# Installation Guide
## Official Packages

| Operating System | Download | MD5 Checksum|
|-------|-----|--------|
| Windows (>=10)| [⏬ Download](https://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/v1.1.4/Mogan-v1.1.4-64bit-installer.exe) | 36dcac15825bbb527de51e348d18df7a |
| macOS (>=11) | [⏬ Download](https://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/v1.1.4/Mogan_v1.1.4.dmg) | 6224ba3a809a1d20ee118c20ee06d4a6 |
| macOS arm (>=12.6) | [⏬ Download](https://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/v1.1.4/Mogan_arm_v1.1.4.dmg) | bac4d3acb670905acea567f28c1e84f8 |
| Ubuntu 20.04 | [⏬ Download](https://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/v1.1.4/mogan-v1.1.4-ubuntu20.04.deb) | 722add12fe4aeba0ed8c3c7bef10f79a |
| Ubuntu 22.04 | [⏬ Download](https://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/v1.1.4/mogan-v1.1.4-ubuntu22.04.deb) | d45cca14fa8e36f55f5e2c1c1dffe76d |
| UOS amd64 | [⏬ Download](https://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/v1.1.4/mogan-v1.1.4-uos.deb) | e65a45c5e836fb8949cfb2f9f2858725 |
| UOS loongarch | [⏬ Download](https://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/v1.1.4/mogan-v1.1.4-uos-loongarch64.deb) | e1b2a8a63912ea28980b5becb80a2f49 |

Other sites to download：
+ https://codeberg.org/XmacsLabs/mogan/releases
+ https://gitee.com/XmacsLabs/mogan/releases
+ https://github.com/XmacsLabs/mogan/releases

Old releases:
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

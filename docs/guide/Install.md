# Installation Guide
## Official Packages
| Operating System | Download | MD5 Checksum|
|-------|-----|--------|
| Windows (>=10)| [⏬ Download](https://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/Mogan%20Editor%20v1.1.3/Mogan-v1.1.3-64bit-installer.exe) | f6d46f104156f83f72e9e93122b17797 |
| macOS (>=10.15) | [⏬ Download](https://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/Mogan%20Editor%20v1.1.3/Mogan_v1.1.3.dmg) | 51ab76ba5ade77d604b6956131f4d489 |
| macOS arm (>=12.6) | [⏬ Download](https://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/Mogan%20Editor%20v1.1.3/Mogan_arm_v1.1.3.dmg) | af6a5ee2b9237891feaf698148a7bca0 |
| Ubuntu 20.04 | [⏬ Download](https://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/Mogan%20Editor%20v1.1.3/mogan-v1.1.3-ubuntu20.04.deb) | 1aa101ff1c5ba74212a5d81d0c647a2e |
| Ubuntu 22.04 | [⏬ Download](https://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/Mogan%20Editor%20v1.1.3/mogan-v1.1.3-ubuntu22.04.deb) | 6e29e788f24b57cb742c5b0ec1cef0c8 |
| UOS amd64 | [⏬ Download](https://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/Mogan%20Editor%20v1.1.3/mogan_1.1.3-uos-amd64.deb) | ea09c29cfbc3d81ebb9156855e89fa1c |
| UOS loongarch | [⏬ Download](https://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/Mogan%20Editor%20v1.1.3/mogan_1.1.3-uos-loongarch64.deb) | 8837ce391de7558cfb387db74ff9edb2 |

Other sites to download：
+ https://codeberg.org/XmacsLabs/mogan/releases
+ https://gitee.com/XmacsLabs/mogan/releases
+ https://github.com/XmacsLabs/mogan/releases

Old releases:
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

# Installation Guide
## Official Packages
| Operating System | Download | MD5 Checksum|
|-------|-----|--------|
| Windows (>=10)| [Gitee](https://gitee.com/XmacsLabs/mogan/attach_files/1370402/download)/[Github](https://github.com/XmacsLabs/mogan/releases/download/v1.1.2/Mogan-v1.1.2-64bit-installer.exe) | a3ad8bccb60e161bb733b125949eeadd |
| macOS (>=10.15) | [Gitee](https://gitee.com/XmacsLabs/mogan/attach_files/1370400/download)/[Github](https://github.com/XmacsLabs/mogan/releases/download/v1.1.2/Mogan_v1.1.2.dmg) | 70a5e1107445fed61543c01bce053224 |
| macOS arm (>=12.6) | [Gitee](https://gitee.com/XmacsLabs/mogan/attach_files/1370409/download)/[Github](https://github.com/XmacsLabs/mogan/releases/download/v1.1.2/Mogan_arm_v1.1.2.dmg) | b5d56e8faef9d94867624c7f0f196c32 |
| Ubuntu 20.04 | [Gitee](https://gitee.com/XmacsLabs/mogan/attach_files/1370401/download)/[Github](https://github.com/XmacsLabs/mogan/releases/download/v1.1.2/mogan-v1.1.2-ubuntu20.04.deb) | 1c8e3664deb0606766310ec323b3777e |
| Ubuntu 22.04 | [Gitee](https://gitee.com/XmacsLabs/mogan/attach_files/1370403/download)/[Github](https://github.com/XmacsLabs/mogan/releases/download/v1.1.2/mogan-v1.1.2-ubuntu22.04.deb) | acadc8feec924238a4faafbdac9f5f51 |

Old releases:
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

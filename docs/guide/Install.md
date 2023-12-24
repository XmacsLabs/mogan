# Installation Guide
The latest stable version is Mogan STEM Suite v1.2.2, you can check if there are new releases in the `Help -> Welcome` menu entry.

## Official Packages
| Product | Operating System | Download | SHA256 Checksum|
|---------|-------|-----|--------|
| Mogan Research v1.2.2 | Windows (>=10)| [⏬ Download](https://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/v1.2.2/MoganResearch-v1.2.2-64bit-installer.exe) | 8b8e2cc54c64436b965417c9f76beaded181d3c4a192f2eae62189d88b754e28 |
| Mogan Research v1.2.2 | macOS (>=11) | [⏬ Download](https://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/v1.2.2/MoganResearch-v1.2.2.dmg) | 6470306b334f2da09a6be515ba2189c639954ae53333c0213074154364d137e7 |
| Mogan Research v1.2.2 | macOS M1/M2 (>=13) | [⏬ Download](https://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/v1.2.2/MoganResearch-v1.2.2-arm.dmg) | ccbd7843542cd94c4a5315694c62c7f5dbdb81e66542820d649e1328c42fc8b5 |
| Mogan Research v1.2.2 | Ubuntu 22.04 | [⏬ Download](https://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/v1.2.2/mogan-research-v1.2.2-ubuntu22.04.deb) | dc9f3d1e18afb4f27b598c8251815ad1082f14c58e6685e218965c63e6d19151 |
| Mogan Research v1.2.2 | UOS | [⏬ Download](https://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/v1.2.2/mogan-research-v1.2.2-uos.deb) | 9ae3f63c7b562fc51a408d97ad3f253fe11aa30a348f8212e277b3e1d5a1bc10 |
| Mogan Research v1.2.2 | UOS Loongarch | [⏬ Download](https://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/v1.2.2/mogan-research-v1.2.2-uos-loongarch64.deb) | 5185047d2acbf83465ec5d28406133512a7b8c6d450b70d08062b34c9dc59af1 |

Mogan STEM Suite:
+ **Mogan Research** (installers on Windows/macOS/Ubuntu/UOS)
+ Mogan Code (no installer, still in development)
+ Mogan Beamer (no installer, still in development)


Other sites to download：
+ https://github.com/XmacsLabs/mogan/releases
+ https://gitee.com/XmacsLabs/mogan/releases
+ https://codeberg.org/XmacsLabs/mogan/releases

Old releases:
+ [v1.2.1 (2023/12/21)](https://github.com/XmacsLabs/mogan/releases/tag/v1.2.1)
+ [v1.2.0 (2023/12/04)](https://github.com/XmacsLabs/mogan/releases/tag/v1.2.0)
+ [**v1.1.6 (2023/09/29)**](https://github.com/XmacsLabs/mogan/releases/tag/v1.1.6)
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

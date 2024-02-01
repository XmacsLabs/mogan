# Installation Guide
The latest stable version is Mogan STEM Suite v1.2.4, you can check if there are new releases in the `Help -> Welcome` menu entry.

## Official Packages
| Product | Operating System | Download |
|---------|-------|-----|
| Mogan Research v1.2.4 | Windows installer (>=10)| [⏬ Download](https://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/v1.2.4/MoganResearch-v1.2.4-64bit-installer.exe) |
| Mogan Research v1.2.4 | Windows portable (>=10)| [⏬ Download](https://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/v1.2.4/MoganResearch-v1.2.4-64bit-installer.zip) |
| Mogan Research v1.2.4 | macOS (>=11) | [⏬ Download](https://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/v1.2.4/MoganResearch-v1.2.4.dmg) |
| Mogan Research v1.2.4 | macOS M1/M2 (>=13) | [⏬ Download](https://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/v1.2.4/MoganResearch-v1.2.4-arm.dmg) |
| Mogan Research v1.2.4 | Ubuntu 22.04 | [⏬ Download](https://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/v1.2.4/mogan-research-v1.2.4-ubuntu22.04.deb) |

SHA256 checksum:
```
c66ff1721e418ccfc8733119a970820675c16dba034fd1b599d928726f61f673  MoganResearch-v1.2.4-64bit-installer.exe
f4a5b46a67581b864d7cf58df4d4a3c6a6bcef28c6e7419f8729bc0529cf224d  MoganResearch-v1.2.4-64bit-installer.zip
21dabce305d35de338aa7453fcd6e146a8d3df2e6792f7617f2092c26b7bcc82  MoganResearch-v1.2.4-arm.dmg
d80bdc9ba4d1a028ca947e1d577ba1ffae3e4bae57471d020c87262b945c4166  MoganResearch-v1.2.4.dmg
9f95cc572a3b75f2f3d3757c2625477ec97c1361bba3dbcddf5dfe13371a00a3  mogan-research-v1.2.4-ubuntu22.04.deb
```

Mogan STEM Suite:
+ **Mogan Research** (installers on Windows/macOS/Ubuntu)
+ Mogan Code (no installer, still in development)
+ Mogan Beamer (no installer, still in development)


Other sites to download：
+ https://github.com/XmacsLabs/mogan/releases
+ https://gitee.com/XmacsLabs/mogan/releases
+ https://codeberg.org/XmacsLabs/mogan/releases

## Recommended old releases
Somtimes, you may prefer older releases:
+ [**v1.1.6**](https://github.com/XmacsLabs/mogan/releases/tag/v1.1.6)
+ [**v1.1.3**](https://github.com/XmacsLabs/mogan/releases/tag/v1.1.3)

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

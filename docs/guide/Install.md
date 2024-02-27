# Installation Guide
The latest stable version is Mogan STEM Suite v1.2.5, you can check if there are new releases in the `Help -> Welcome` menu entry.

## Official Packages
| Product | Operating System | Download |
|---------|-------|-----|
| Mogan Research v1.2.5 | Windows installer (>=10)| [⏬ Download](https://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/v1.2.5/MoganResearch-v1.2.5-64bit-installer.exe) |
| Mogan Research v1.2.5 | Windows portable (>=10)| [⏬ Download](https://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/v1.2.5/MoganResearch-v1.2.5-64bit-installer.zip) |
| Mogan Research v1.2.5 | macOS (>=11) | [⏬ Download](https://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/v1.2.5/MoganResearch-v1.2.5.dmg) |
| Mogan Research v1.2.5 | macOS M1/M2 (>=13) | [⏬ Download](https://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/v1.2.5/MoganResearch-v1.2.5-arm.dmg) |
| Mogan Research v1.2.5 | Ubuntu 22.04 | [⏬ Download](https://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/v1.2.5/mogan-research-v1.2.5-ubuntu22.04.deb) |
| 墨干v1.2.5 | Debian 12 | [⏬ Download](http://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/v1.2.5/mogan-research-v1.2.5-debian12.deb) |

SHA256 checksum:
```
f4d485b2add98b8d9618b2a82ba3f6720fad78fcdefc91eb6263d3cb4c39a370  MoganResearch-v1.2.5-64bit-installer.exe
456219c8ca3db8ceac4a5a9f47385d56405f06c32a09d3ffd9acc7ba34d35ac9  MoganResearch-v1.2.5-64bit-installer.zip
8164c4e7766c72a0010d7db7abd62d338d8ede7aca22bc5550391937d7d301f5  MoganResearch-v1.2.5-arm.dmg
361354aad2a5de93d22508bb784c0e940a43d28d559dda79803499f4fb3c0dc6  MoganResearch-v1.2.5.dmg
498c232ec032d41d8cfa3edc10814ff7a0015818fe43fe8542f9807e2ec04b18  mogan-research-v1.2.5-debian12.deb
558ad6a1dd4f384d33b0dfed9a1171ebcccdb71f417173a2f0f010d838c35dbf  mogan-research-v1.2.5-ubuntu20.04.deb
a147c7630b99592f77437172de3245e11f930bff23c61c622d0a03a4b0104daa  mogan-research-v1.2.5-ubuntu22.04.deb
```

Mogan STEM Suite:
+ **Mogan Research** (installers on Windows/macOS/Ubuntu/Debian)
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

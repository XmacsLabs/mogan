# Installation Guide
The latest stable version is Mogan STEM Suite v1.2.5.3 LTS, you can check if there are new releases in the `Help -> Welcome` menu entry.

## Official Packages
| Product | Operating System | Download |
|---------|-------|-----|
| Mogan Research v1.2.5.3 | Windows installer (>=10)| [⏬ Download](https://github.com/XmacsLabs/mogan/releases/download/v1.2.5.3/MoganResearch-v1.2.5.3-64bit-installer.exe) |
| Mogan Research v1.2.5.3 | Windows portable (>=10)| [⏬ Download](https://github.com/XmacsLabs/mogan/releases/download/v1.2.5.3/MoganResearch-v1.2.5.3-64bit-installer.zip) |
| Mogan Research v1.2.5.3 | macOS (>=12) | [⏬ Download](https://github.com/XmacsLabs/mogan/releases/download/v1.2.5.3/MoganResearch-v1.2.5.3.dmg) |
| Mogan Research v1.2.5.3 | macOS arm64 (>=13) | [⏬ Download](https://github.com/XmacsLabs/mogan/releases/download/v1.2.5.3/MoganResearch-v1.2.5.3-arm.dmg) |
| Mogan Research v1.2.5.3 | Ubuntu 20.04 | [⏬ Download](https://github.com/XmacsLabs/mogan/releases/download/v1.2.5.3/mogan-research-v1.2.5.3-ubuntu20.04.deb) |
| Mogan Research v1.2.5.3 | Ubuntu 22.04 | [⏬ Download](https://github.com/XmacsLabs/mogan/releases/download/v1.2.5.3/mogan-research-v1.2.5.3-ubuntu22.04.deb) |
| Mogan Research v1.2.5.3 | Debian 12 | [⏬ Download](https://github.com/XmacsLabs/mogan/releases/download/v1.2.5.3/mogan-research-v1.2.5.3-debian12.deb) |

SHA256 checksum:
```
71544fa036ae941f9a5b29574f05d1120e05ea44d27ce24a9f197711741ed32a  MoganResearch-v1.2.5.3-64bit-installer.exe
741adce5925314c9daaaeaf0c60dde691fc7737080567da1757e0884fa28dc7e  MoganResearch-v1.2.5.3-64bit-installer.zip
7839ee6df89e4d4fb4ed03d60893dbbccedf0d45e3b5cbaaf1adfb7a01e86f9f  MoganResearch-v1.2.5.3-arm.dmg
eb235b4013b5cb5752d465bee54b85c48466665bd16e12652c6b025e289725ed  MoganResearch-v1.2.5.3.dmg
b5134d8bc28c2b56ab006ba90e4ef5419846fb72ff33e5daf1d294a0dcd8d308  mogan-research-v1.2.5.3-debian12.deb
0c0736eb42bfab5d80f6c1f00a958dcae75cde659f25b662cd46eeb90c4652c2  mogan-research-v1.2.5.3-ubuntu20.04.deb
4b0a9aaad332a2bb4fc670f1a8de588f79dae8c17ebae6202e257d2f6e8434bf  mogan-research-v1.2.5.3-ubuntu22.04.deb
```

Mogan STEM Suite:
+ **Mogan Research** (installers on Windows/macOS/Ubuntu/Debian)
+ Mogan Code (no installer, still in development)
+ Mogan Beamer (no installer, still in development)


Other sites to download：
+ https://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/
+ https://github.com/XmacsLabs/mogan/releases
+ https://gitee.com/XmacsLabs/mogan/releases
+ https://codeberg.org/XmacsLabs/mogan/releases

Here are installation steps for various system:

## Debian 12
Download the official deb and then:
```
sudo apt install ./mogan-research-v1.2.5.3-debian12.deb
```
## Ubuntu 20.04/Ubuntu 22.04
Download the official deb and then
```
# For ubuntu 20.04
sudo apt install ./mogan-research-v1.2.5.3-ubuntu20.04.deb

# For ubuntu 22.04
sudo apt install ./mogan-research-v1.2.5.3-ubuntu22.04.deb
```

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

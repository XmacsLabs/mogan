# Installation Guide
The latest stable version is Mogan STEM Suite v1.2.5.4 LTS, you can check if there are new releases in the `Help -> Welcome` menu entry.

## Official Packages
| Product | Operating System | Download |
|---------|-------|-----|
| Mogan Research v1.2.5.4 | Windows installer (>=10)| [⏬ Download](https://github.com/XmacsLabs/mogan/releases/download/v1.2.5.4/MoganResearch-v1.2.5.4-64bit-installer.exe) |
| Mogan Research v1.2.5.4 | Windows portable (>=10)| [⏬ Download](https://github.com/XmacsLabs/mogan/releases/download/v1.2.5.4/MoganResearch-v1.2.5.4-64bit-installer.zip) |
| Mogan Research v1.2.5.4 | macOS (>=12) | [⏬ Download](https://github.com/XmacsLabs/mogan/releases/download/v1.2.5.4/MoganResearch-v1.2.5.4.dmg) |
| Mogan Research v1.2.5.4 | macOS arm64 (>=13) | [⏬ Download](https://github.com/XmacsLabs/mogan/releases/download/v1.2.5.4/MoganResearch-v1.2.5.4-arm.dmg) |
| Mogan Research v1.2.5.4 | Ubuntu 20.04 | [⏬ Download](https://github.com/XmacsLabs/mogan/releases/download/v1.2.5.4/mogan-research-v1.2.5.4-ubuntu20.04.deb) |
| Mogan Research v1.2.5.4 | Ubuntu 22.04 | [⏬ Download](https://github.com/XmacsLabs/mogan/releases/download/v1.2.5.4/mogan-research-v1.2.5.4-ubuntu22.04.deb) |
| Mogan Research v1.2.5.4 | Debian 12 | [⏬ Download](https://github.com/XmacsLabs/mogan/releases/download/v1.2.5.4/mogan-research-v1.2.5.4-debian12.deb) |

SHA256 checksum:
```
3a77620e81a00fd3879f5c6bca1a82abe158a352a439400b751823e5527f27db  MoganResearch-v1.2.5.4-64bit-installer.exe
21f8555a3360653afc5af56908b6b24a3127a25cfec081dd3df1b013a15dda4d  MoganResearch-v1.2.5.4-64bit-installer.zip
371ccc5fc23d6f4e59915796fc02466a702a4e7626cfcfe25090757c60ba6ab7  MoganResearch-v1.2.5.4-arm.dmg
cabeb34ca31257e0361865bd6f46010682983c00d1288e67433670946c0f344d  MoganResearch-v1.2.5.4.dmg
41cc47ac5bb51f87bbc52eb6b7578adc59f590d94d8b40b5edf50ed91d262905  mogan-research-v1.2.5.4-debian12.deb
8482544117cfd5ec5cbe2d97ed55cddbda68f0443b34706129d85cce0acc4e71  mogan-research-v1.2.5.4-ubuntu20.04.deb
d9ecf0a3486bdb86781e49bda0d83b0b7b64822126964c69f39ac0c9ef5b1f3c  mogan-research-v1.2.5.4-ubuntu22.04.deb
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
sudo apt install ./mogan-research-v1.2.5.4-debian12.deb
```
## Ubuntu 20.04/Ubuntu 22.04
Download the official deb and then
```
# For ubuntu 20.04
sudo apt install ./mogan-research-v1.2.5.4-ubuntu20.04.deb

# For ubuntu 22.04
sudo apt install ./mogan-research-v1.2.5.4-ubuntu22.04.deb
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

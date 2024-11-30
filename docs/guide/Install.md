# Installation Guide
The latest stable version is Mogan STEM Suite v1.2.9.7, you can check if there are new releases in the `Help -> Welcome` menu entry.

## Official Packages
| Product | Operating System | Download |
|---------|-------|-----|
| Mogan Research v1.2.9.7 | Windows installer (>=10)| [⏬ Download](https://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/v1.2.9.7/MoganResearch-v1.2.9.7-64bit-installer.exe) |
| Mogan Research v1.2.9.7 | Windows portable (>=10)| [⏬ Download](https://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/v1.2.9.7/MoganResearch-v1.2.9.7-64bit-portable.zip) |
| Mogan Research v1.2.9.7 | macOS arm64 (>=13) | [⏬ Download](https://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/v1.2.9.7/MoganResearch-v1.2.9.7-arm.dmg) |
| Mogan Research v1.2.9.7 | macOS (>=12) | [⏬ Download](https://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/v1.2.9.7/MoganResearch-v1.2.9.7.dmg) |
| Mogan Research v1.2.9.7 | Debian 12 | [⏬ Download](https://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/v1.2.9.7/mogan-research-v1.2.9.7-debian12.deb) |
| Mogan Research v1.2.9.7 | Ubuntu 22.04 | [⏬ Download](https://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/v1.2.9.7/mogan-research-v1.2.9.7-ubuntu22.04.deb) |
| Mogan Research v1.2.9.7 | Fedora 41 | [⏬ Download](https://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/v1.2.9.7/mogan-research-v1.2.9.7-fedora41.rpm) |
| Mogan Research v1.2.9.7 | Ubuntu 24.04 | [⏬ Download](https://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/v1.2.9.7/mogan-research-v1.2.9.7-ubuntu24.04.deb) |

SHA256 checksum:
```
3eeb3f15b930110b22febe3a27e937efaceaa7821d62c6b73a1f17484825dc46  MoganResearch-v1.2.9.7-64bit-installer.exe
c51a9c0f801f23e1ba886f7133277aefc213932b8495abb17168a6ff4604dc36  MoganResearch-v1.2.9.7-64bit-portable.zip
4b2dbfe74492031f12760701c5f878b98bb1fb2abc2e449f7d692ba2dc4b9939  MoganResearch-v1.2.9.7-arm.dmg
a31276e0091b008e3ff9a2f3f1a6e00828221371fe073cb53d2760d349af4fa0  MoganResearch-v1.2.9.7.dmg
810a0d341bab1b58f05e73f759416aee944f72821c9121884bf6df0f65bd51cf  mogan-research-v1.2.9.7-debian12.deb
fc6171d44e29351c2bd16fffbbb18ed287ef20b8871f7b9ca2ce898587dd948c  mogan-research-v1.2.9.7-fedora41.rpm
f4bcb8d4a9bec63b158c93fc0efbc083da95de84ddf7fb84ecf56675120458e5  mogan-research-v1.2.9.7-ubuntu22.04.deb
6716064ec9f570aad3cbd8a84251d06cbbc7055f4875b323d2c7718a39ac9231  mogan-research-v1.2.9.7-ubuntu24.04.deb
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
sudo apt install ./mogan-research-v1.2.9.7-debian12.deb
```
## Ubuntu 22.04/Ubuntu 24.04
Download the official deb and then
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

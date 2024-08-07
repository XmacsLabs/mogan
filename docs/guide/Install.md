# Installation Guide
The latest stable version is Mogan STEM Suite v1.2.8, you can check if there are new releases in the `Help -> Welcome` menu entry.

## Official Packages
| Product | Operating System | Download |
|---------|-------|-----|
| Mogan Research v1.2.8 | Windows installer (>=10)| [⏬ Download](https://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/v1.2.8/MoganResearch-v1.2.8-64bit-installer.exe) |
| Mogan Research v1.2.8 | Windows portable (>=10)| [⏬ Download](https://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/v1.2.8/MoganResearch-v1.2.8-64bit-installer.zip) |
| Mogan Research v1.2.8 | macOS arm64 (>=13) | [⏬ Download](https://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/v1.2.8/MoganResearch-v1.2.8-arm.dmg) |
| Mogan Research v1.2.8 | macOS (>=12) | [⏬ Download](https://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/v1.2.8/MoganResearch-v1.2.8.dmg) |
| Mogan Research v1.2.8 | Debian 12 | [⏬ Download](https://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/v1.2.8/mogan-research-v1.2.8-debian12.deb) |
| Mogan Research v1.2.8 | Ubuntu 22.04 | [⏬ Download](https://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/v1.2.8/mogan-research-v1.2.8-ubuntu22.04.deb) |
| Mogan Research v1.2.8 | Ubuntu 24.04 | [⏬ Download](https://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/v1.2.8/mogan-research-v1.2.8-ubuntu24.04.deb) |

SHA256 checksum:
```
6bd14e6f2f7f9eb12c5fe5dd42617b48dec33b879592bdd2774ee0dbcfb3745e  MoganResearch-v1.2.8-64bit-installer.exe
9c691e0d187f67d3378df27fafd8024abc75b068b2afe87a3e05ba83fb43a1b9  MoganResearch-v1.2.8-64bit-installer.zip
d2dc977ff90ca2204db66017dc47a9457487b9b9d2d664858ac0f951e963e05a  MoganResearch-v1.2.8-arm.dmg
49700360dc2ec131c676564d523361bf1e53421fb8bc1540e1e84f846a982a95  MoganResearch-v1.2.8.dmg
8f8982158c98fed2baa421f2212fa8f1cd21407e77f9cb90468928578f4fad5d  mogan-research-v1.2.8-debian12.deb
aa0f942b7d0dd1e9e3db0687bf978857cc4ade3500a0f80e84d4461632e39ba1  mogan-research-v1.2.8-ubuntu22.04.deb
b2ca3008ceaeca12d528470a8e6f1375b9c1d814ace4fb09bc4745f6dbe3194e  mogan-research-v1.2.8-ubuntu24.04.deb
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
sudo apt install ./mogan-research-v1.2.8-debian12.deb
```
## Ubuntu 22.04/Ubuntu 24.04
Download the official deb and then
```
# For ubuntu 22.04
sudo apt install ./mogan-research-v1.2.8-ubuntu22.04.deb

# For ubuntu 24.04
sudo apt install ./mogan-research-v1.2.8-ubuntu24.04.deb
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

# Installation Guide
The latest stable version is Mogan STEM Suite v1.2.9.3, you can check if there are new releases in the `Help -> Welcome` menu entry.

## Official Packages
| Product | Operating System | Download |
|---------|-------|-----|
| Mogan Research v1.2.9.3 | Windows installer (>=10)| [⏬ Download](https://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/v1.2.9.3/MoganResearch-v1.2.9.3-64bit-installer.exe) |
| Mogan Research v1.2.9.3 | Windows portable (>=10)| [⏬ Download](https://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/v1.2.9.3/MoganResearch-v1.2.9.3-64bit-portable.zip) |
| Mogan Research v1.2.9.3 | macOS arm64 (>=13) | [⏬ Download](https://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/v1.2.9.3/MoganResearch-v1.2.9.3-arm.dmg) |
| Mogan Research v1.2.9.3 | macOS (>=12) | [⏬ Download](https://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/v1.2.9.3/MoganResearch-v1.2.9.3.dmg) |
| Mogan Research v1.2.9.3 | Debian 12 | [⏬ Download](https://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/v1.2.9.3/mogan-research-v1.2.9.3-debian12.deb) |
| Mogan Research v1.2.9.3 | Ubuntu 22.04 | [⏬ Download](https://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/v1.2.9.3/mogan-research-v1.2.9.3-ubuntu22.04.deb) |
| Mogan Research v1.2.9.3 | Ubuntu 24.04 | [⏬ Download](https://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/v1.2.9.3/mogan-research-v1.2.9.3-ubuntu24.04.deb) |

SHA256 checksum:
```
ca0662525915a4d3983c4891d984526f5225fd63352b44f60db7b679f293becc  MoganResearch-v1.2.9.3-64bit-installer.exe
3b51f7bbb3e90c6a542010700095d7517db61d25f5e6f1b3d7adf8b44345c624  MoganResearch-v1.2.9.3-64bit-portable.zip
74cb464ddde67d8c717c87362d5ad4b29c095be19de85ee9363f6b459ad05049  MoganResearch-v1.2.9.3-arm.dmg
25bc5fc683bcc60ee720a5995b8ad5990fd702365baea5e7d0590973675cf746  MoganResearch-v1.2.9.3.dmg
ac21d63d4e1f550b3485939030658ac2980014240d9b7b77a080013b762f9b91  mogan-research-v1.2.9.3-debian12.deb
6f1fc4342471e3e1493976e56a30becc59c394569baa603cd2054005c02106a3  mogan-research-v1.2.9.3-ubuntu22.04.deb
a287a607caaa188b0b5ba96cedf333c1da9d8b82261c163861ea8e4ef5f7f081  mogan-research-v1.2.9.3-ubuntu24.04.deb
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
sudo apt install ./mogan-research-v1.2.9.3-debian12.deb
```
## Ubuntu 22.04/Ubuntu 24.04
Download the official deb and then
```
# For ubuntu 22.04
sudo apt install ./mogan-research-v1.2.9.3-ubuntu22.04.deb

# For ubuntu 24.04
sudo apt install ./mogan-research-v1.2.9.3-ubuntu24.04.deb
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

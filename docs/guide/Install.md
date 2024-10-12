# Installation Guide
The latest stable version is Mogan STEM Suite v1.2.9.2, you can check if there are new releases in the `Help -> Welcome` menu entry.

## Official Packages
| Product | Operating System | Download |
|---------|-------|-----|
| Mogan Research v1.2.9.2 | Windows installer (>=10)| [⏬ Download](https://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/v1.2.9.2/MoganResearch-v1.2.9.2-64bit-installer.exe) |
| Mogan Research v1.2.9.2 | Windows portable (>=10)| [⏬ Download](https://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/v1.2.9.2/MoganResearch-v1.2.9.2-64bit-portable.zip) |
| Mogan Research v1.2.9.2 | macOS arm64 (>=13) | [⏬ Download](https://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/v1.2.9.2/MoganResearch-v1.2.9.2-arm.dmg) |
| Mogan Research v1.2.9.2 | macOS (>=12) | [⏬ Download](https://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/v1.2.9.2/MoganResearch-v1.2.9.2.dmg) |
| Mogan Research v1.2.9.2 | Debian 12 | [⏬ Download](https://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/v1.2.9.2/mogan-research-v1.2.9.2-debian12.deb) |
| Mogan Research v1.2.9.2 | Ubuntu 22.04 | [⏬ Download](https://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/v1.2.9.2/mogan-research-v1.2.9.2-ubuntu22.04.deb) |
| Mogan Research v1.2.9.2 | Ubuntu 24.04 | [⏬ Download](https://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/v1.2.9.2/mogan-research-v1.2.9.2-ubuntu24.04.deb) |

SHA256 checksum:
```
700eeb39fa4f5eaba4ee8839f6a813488a8f5f758722f4fb0e1c5c0c7240a70e  MoganResearch-v1.2.9.2-64bit-installer.exe
469f1e48e2a780345bf359157b27ef43097f3883b7f27e58041a144cc0cc990b  MoganResearch-v1.2.9.2-64bit-portable.zip
cbca96dfc2f1073762cebf56c642964b37802b5bbe9e6bac3475cfe6962e6968  MoganResearch-v1.2.9.2-arm.dmg
aa429c0e656ccd2309ce0bdc1e3087a605052c66c205ec3bf37f5fb812aed19e  MoganResearch-v1.2.9.2.dmg
9266c44775e0d3539adfb9816c45a326cbb74da27ea267bba8989234b5907566  mogan-research-v1.2.9.2-debian12.deb
04878ac96b25af8ad2ab9c2551713cef7d9e2e650460f0f2188b6f103faeb611  mogan-research-v1.2.9.2-ubuntu22.04.deb
aab31ce801d4d9dca9abe4c4ac4f5eddcc18e16934c6e16a80831562a520b1cb  mogan-research-v1.2.9.2-ubuntu24.04.deb
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
sudo apt install ./mogan-research-v1.2.9.2-debian12.deb
```
## Ubuntu 22.04/Ubuntu 24.04
Download the official deb and then
```
# For ubuntu 22.04
sudo apt install ./mogan-research-v1.2.9.2-ubuntu22.04.deb

# For ubuntu 24.04
sudo apt install ./mogan-research-v1.2.9.2-ubuntu24.04.deb
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

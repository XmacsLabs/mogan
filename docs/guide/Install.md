# Installation Guide
The latest stable version is Mogan STEM Suite v1.2.9.5, you can check if there are new releases in the `Help -> Welcome` menu entry.

## Official Packages
| Product | Operating System | Download |
|---------|-------|-----|
| Mogan Research v1.2.9.5 | Windows installer (>=10)| [⏬ Download](https://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/v1.2.9.5/MoganResearch-v1.2.9.5-64bit-installer.exe) |
| Mogan Research v1.2.9.5 | Windows portable (>=10)| [⏬ Download](https://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/v1.2.9.5/MoganResearch-v1.2.9.5-64bit-portable.zip) |
| Mogan Research v1.2.9.5 | macOS arm64 (>=13) | [⏬ Download](https://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/v1.2.9.5/MoganResearch-v1.2.9.5-arm.dmg) |
| Mogan Research v1.2.9.5 | macOS (>=12) | [⏬ Download](https://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/v1.2.9.5/MoganResearch-v1.2.9.5.dmg) |
| Mogan Research v1.2.9.5 | Debian 12 | [⏬ Download](https://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/v1.2.9.5/mogan-research-v1.2.9.5-debian12.deb) |
| Mogan Research v1.2.9.5 | Ubuntu 22.04 | [⏬ Download](https://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/v1.2.9.5/mogan-research-v1.2.9.5-ubuntu22.04.deb) |
| Mogan Research v1.2.9.5 | Ubuntu 24.04 | [⏬ Download](https://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/v1.2.9.5/mogan-research-v1.2.9.5-ubuntu24.04.deb) |

SHA256 checksum:
```
26076445719ae6257153b7c1a7f08f28fe648168ba8ece77f2812768d6de0fe4  MoganResearch-v1.2.9.5-64bit-installer.exe
4fa8de7ce8a7b6b4294f208d3d79e9a8b854f278796312bc0fa926237c4f8f95  MoganResearch-v1.2.9.5-64bit-portable.zip
0f87be214fa4ec030585b2068c9f8066b2bb8e753c6539b37c52c6e73fcf13c9  MoganResearch-v1.2.9.5-arm.dmg
08266def7a60ee75a6b4e0b0a21e8a2bfc3d74ed1a20f2c4a8e3b53a3b6c7c10  MoganResearch-v1.2.9.5.dmg
e81f9dbeeb816e56c0ddb61b36757737b382d50ebc34dfe11b2042ca1eff91c7  mogan-research-v1.2.9.5-debian12.deb
0d4ee6923dc9e527aabc12440a858d5fcce4c28d9b82c8047ab09f2e1f5b57e4  mogan-research-v1.2.9.5-ubuntu22.04.deb
a97f622ec0b48b81e35681eecbf0619d43481f0f59a050ee6e8fa0fbc982fac5  mogan-research-v1.2.9.5-ubuntu24.04.deb
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
sudo apt install ./mogan-research-v1.2.9.5-debian12.deb
```
## Ubuntu 22.04/Ubuntu 24.04
Download the official deb and then
```
# For ubuntu 22.04
sudo apt install ./mogan-research-v1.2.9.5-ubuntu22.04.deb

# For ubuntu 24.04
sudo apt install ./mogan-research-v1.2.9.5-ubuntu24.04.deb
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

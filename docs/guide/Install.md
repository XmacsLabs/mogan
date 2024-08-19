# Installation Guide
The latest stable version is Mogan STEM Suite v1.2.9, you can check if there are new releases in the `Help -> Welcome` menu entry.

## Official Packages
| Product | Operating System | Download |
|---------|-------|-----|
| Mogan Research v1.2.9 | Windows installer (>=10)| [⏬ Download](https://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/v1.2.9/MoganResearch-v1.2.9-64bit-installer.exe) |
| Mogan Research v1.2.9 | Windows portable (>=10)| [⏬ Download](https://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/v1.2.9/MoganResearch-v1.2.9-64bit-portable.zip) |
| Mogan Research v1.2.9 | macOS arm64 (>=13) | [⏬ Download](https://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/v1.2.9/MoganResearch-v1.2.9-arm.dmg) |
| Mogan Research v1.2.9 | macOS (>=12) | [⏬ Download](https://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/v1.2.9/MoganResearch-v1.2.9.dmg) |
| Mogan Research v1.2.9 | Debian 12 | [⏬ Download](https://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/v1.2.9/mogan-research-v1.2.9-debian12.deb) |
| Mogan Research v1.2.9 | Ubuntu 22.04 | [⏬ Download](https://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/v1.2.9/mogan-research-v1.2.9-ubuntu22.04.deb) |
| Mogan Research v1.2.9 | Ubuntu 24.04 | [⏬ Download](https://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/v1.2.9/mogan-research-v1.2.9-ubuntu24.04.deb) |

SHA256 checksum:
```
ac4132447861da8e0cc94646900db11c5f2ead8d68b3b67a7517e49a32ecf4e7  MoganResearch-v1.2.9-64bit-installer.exe
16ce3e3ea3dd7f17c3a9c621d0ff40b281240d81b1f88a1026159d52bc54622d  MoganResearch-v1.2.9-64bit-portable.zip
bc3794c246549d67a6ee0d07b6f4c95a3685c7b6647e842a82876f31d66dd2ca  MoganResearch-v1.2.9-arm.dmg
f12cb7d30f120a8a796b4a666068dd93c30a8c290850beb393ffd21300c4ad21  MoganResearch-v1.2.9.dmg
30db47dfde5122fd72fd0fcee3bf5eaf0138fc8ed607a12e3d1d26122c4529f1  mogan-research-v1.2.9-debian12.deb
09e0a6e2eaad4077109792ebcb14d9cbd67a656aa11f550263cb45c3c0cc86e4  mogan-research-v1.2.9-ubuntu22.04.deb
f720337333f025fa31de1ffc60ad8e488049149d3fa00868b6a7ffcd0015db8b  mogan-research-v1.2.9-ubuntu24.04.deb
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
sudo apt install ./mogan-research-v1.2.9-debian12.deb
```
## Ubuntu 22.04/Ubuntu 24.04
Download the official deb and then
```
# For ubuntu 22.04
sudo apt install ./mogan-research-v1.2.9-ubuntu22.04.deb

# For ubuntu 24.04
sudo apt install ./mogan-research-v1.2.9-ubuntu24.04.deb
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

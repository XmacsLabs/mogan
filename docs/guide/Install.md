# Installation Guide
The latest stable version is Mogan STEM Suite v1.2.1, you can check if there are new releases in `Help -> Welcome`.

## Know Issues of v1.2.1
+ It will crash when searching or replacing in a structural way in the separated window
  + Solution: do not use it, just use the default search/replace toolbar in the bottom
+ It will crash easily when you switch to `Edit -> Preferences -> General -> Buffer Management -> Documents in separate windows`
  + Solution: do not use it, just use the default `Multiple documents share window` buffer management

Mogan STEM Suite v1.2.2 will be released before 2024/01/31.

## Official Packages
| Product | Operating System | Download | MD5 Checksum|
|---------|-------|-----|--------|
| Mogan Research v1.2.1 | Windows (>=10)| [⏬ Download](https://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/v1.2.1/MoganResearch-v1.2.1-64bit-installer.exe) | 619998f9b78541520513f1d9d620c919 |
| Mogan Research v1.2.1 | macOS (>=11) | [⏬ Download](https://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/v1.2.1/MoganResearch-v1.2.1.dmg) | f641259b963a4cc5b754e6972b77bddb |
| Mogan Research v1.2.1 | macOS M1/M2 (>=13) | [⏬ Download](https://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/v1.2.1/MoganResearch-v1.2.1-arm.dmg) | 702f13e33157635d5527be6bd112c3ab |
| Mogan Research v1.2.1 | Ubuntu 22.04 | [⏬ Download](https://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/v1.2.1/mogan-research-v1.2.1-ubuntu22.04.deb) | fb3ac4c59f2eb257adcefefa4340f933 |
| Mogan Research v1.2.1 | UOS | [⏬ Download](https://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/v1.2.1/mogan-research-v1.2.1-uos.deb) | 15126b1a83c4967fea2adbb2bcb7182d |
| Mogan Research v1.2.1 | UOS Loongarch | [⏬ Download](https://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/v1.2.1/mogan-research-v1.2.1-uos-loongarch64.deb) | 8d40fcddd670f8f976f9ca3d7b95526b |

Mogan STEM Suite:
+ **Mogan Research** (installers on Windows/macOS/Ubuntu/UOS)
+ Mogan Code (no installer, still in development)
+ Mogan Beamer (no installer, still in development)


Other sites to download：
+ https://github.com/XmacsLabs/mogan/releases
+ https://gitee.com/XmacsLabs/mogan/releases
+ https://codeberg.org/XmacsLabs/mogan/releases

Old releases:
+ [v1.2.0 (2023/12/04)](https://github.com/XmacsLabs/mogan/releases/tag/v1.2.0)
+ [v1.1.6 (2023/09/29)](https://github.com/XmacsLabs/mogan/releases/tag/v1.1.6)
+ [v1.1.5 (2023/08/11)](https://github.com/XmacsLabs/mogan/releases/tag/v1.1.5)
+ [v1.1.4 (2023/07/31)](https://github.com/XmacsLabs/mogan/releases/tag/v1.1.4)
+ [v1.1.3 (2023/06/05)](https://github.com/XmacsLabs/mogan/releases/tag/v1.1.3)
+ [v1.1.2 (2023/04/09)](https://github.com/XmacsLabs/mogan/releases/tag/v1.1.2)
+ [v1.1.1 (2022/10/29)](https://github.com/XmacsLabs/mogan/releases/tag/v1.1.1)
+ [v1.1.0 (2022/08/31)](https://github.com/XmacsLabs/mogan/releases/tag/v1.1.0)

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

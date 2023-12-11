# Installation Guide
The latest stable version is Mogan STEM Suite v1.2.0.

## Know Issues of v1.2.0
+ Issue: No desktop shortcut created by the Windows installer
  - Solution: Find `C:/Program Files/MoganResearch/bin/MGResearch.exe` and then launch Mogan Research or create the desktop shortcut by yourself
+ Issue: Crash on clicking `Edit->Keyboard->Edit keyboard shortcuts`
  - Solution 1: wait for v1.2.1 and do not click it
  - Solution 2: Use v1.2.1-rc1 https://github.com/XmacsLabs/mogan/releases/tag/v1.2.1-rc1
+ Issue: Crash on `File->Export->LaTeX`
  - Solution 1: wait for v1.2.1 or use `Edit->Copy to->LaTeX` instead
  - Solution 2: Use v1.2.1-rc1 https://github.com/XmacsLabs/mogan/releases/tag/v1.2.1-rc1
+ Issue: Too slow to open Mogan Research v1.2.0 for the first installation
  - Solution: wait for v1.2.1 and be patient

Mogan STEM Suite v1.2.1 will be released before 2024/01/01.

## Official Packages
| Product | Operating System | Download | MD5 Checksum|
|---------|-------|-----|--------|
| Mogan Research v1.2.0 | Windows (>=10)| [⏬ Download](https://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/v1.2.0/MoganResearch-v1.2.0-64bit-installer.exe) | f34dd47aecf9a47fad7c4b27529ef8b4 |
| Mogan Research v1.2.0 | macOS (>=11) | [⏬ Download](https://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/v1.2.0/MoganResearch-v1.2.0.dmg) | cb0e449ae95832c62dac7a16e4fecb9e |
| Mogan Research v1.2.0 | macOS M1/M2 (>=13) | [⏬ Download](https://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/v1.2.0/MoganResearch-v1.2.0-arm.dmg) | 0875c9f252ddacacc4b82df0f341f157 |
| Mogan Research v1.2.0 | Ubuntu 22.04 | [⏬ Download](https://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/v1.2.0/mogan-research-v1.2.0-ubuntu22.04.deb) | c3357688696d060ab451de27bf9364bf |

Mogan STEM Suite:
+ **Mogan Research** (installers on Windows/macOS/Ubuntu)
+ Mogan Code (no installer, still in development)
+ Mogan Beamer (no installer, still in development)


Other sites to download：
+ https://github.com/XmacsLabs/mogan/releases
+ https://gitee.com/XmacsLabs/mogan/releases
+ https://codeberg.org/XmacsLabs/mogan/releases

Old releases:
+ [v1.1.6 (2023/09/29)](https://github.com/XmacsLabs/mogan/releases/tag/v1.1.5)
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

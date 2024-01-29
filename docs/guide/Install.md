# Installation Guide
The latest stable version is Mogan STEM Suite v1.2.3, you can check if there are new releases in the `Help -> Welcome` menu entry.

## Official Packages
| Product | Operating System | Download |
|---------|-------|-----|
| Mogan Research v1.2.3 | Windows (>=10)| [⏬ Download](https://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/v1.2.3/MoganResearch-v1.2.3-64bit-installer.exe) |
| Mogan Research v1.2.3 | macOS (>=11) | [⏬ Download](https://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/v1.2.3/MoganResearch-v1.2.3.dmg) |
| Mogan Research v1.2.3 | macOS M1/M2 (>=13) | [⏬ Download](https://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/v1.2.3/MoganResearch-v1.2.3-arm.dmg) |
| Mogan Research v1.2.3 | Ubuntu 22.04 | [⏬ Download](https://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/v1.2.3/mogan-research-v1.2.3-ubuntu22.04.deb) |

SHA256 checksum:
```
d4559441ec2d9a608d44e69e0146ea3a3ef056e182fe17a828f67b0430d98552  MoganResearch-v1.2.3-64bit-installer.exe
915d8413586106db360914edae75ab57760d6aca738d3896747a31715163e332  MoganResearch-v1.2.3-arm.dmg
ae0b96a46843a95575f313a5c1bec37c9ec8d77573c383163eec97b0ad9f098a  MoganResearch-v1.2.3.dmg
ae4683bebc119443f4803c3bf60f7eff0e9c3c3e19ce6ae8be3f1f2a9b678586  mogan-research-v1.2.3-ubuntu22.04.deb
```

Mogan STEM Suite:
+ **Mogan Research** (installers on Windows/macOS/Ubuntu)
+ Mogan Code (no installer, still in development)
+ Mogan Beamer (no installer, still in development)


Other sites to download：
+ https://github.com/XmacsLabs/mogan/releases
+ https://gitee.com/XmacsLabs/mogan/releases
+ https://codeberg.org/XmacsLabs/mogan/releases

## Recommended releases
+ [**v1.1.6 (2023/09/29)**](https://github.com/XmacsLabs/mogan/releases/tag/v1.1.6)

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

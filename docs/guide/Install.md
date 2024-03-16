# Installation Guide
The latest stable version is Mogan STEM Suite v1.2.5.1 LTS, you can check if there are new releases in the `Help -> Welcome` menu entry.

## Official Packages
| Product | Operating System | Download |
|---------|-------|-----|
| Mogan Research v1.2.5.1 | Windows installer (>=10)| [⏬ Download](https://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/v1.2.5.1/MoganResearch-v1.2.5.1-64bit-installer.exe) |
| Mogan Research v1.2.5.1 | Windows portable (>=10)| [⏬ Download](https://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/v1.2.5.1/MoganResearch-v1.2.5.1-64bit-installer.zip) |
| Mogan Research v1.2.5.1 | macOS (>=11) | [⏬ Download](https://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/v1.2.5.1/MoganResearch-v1.2.5.1.dmg) |
| Mogan Research v1.2.5.1 | macOS M1/M2 (>=13) | [⏬ Download](https://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/v1.2.5.1/MoganResearch-v1.2.5.1-arm.dmg) |
| Mogan Research v1.2.5.1 | Ubuntu 20.04 | [⏬ Download](https://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/v1.2.5.1/mogan-research-v1.2.5.1-ubuntu20.04.deb) |
| Mogan Research v1.2.5.1 | Ubuntu 22.04 | [⏬ Download](https://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/v1.2.5.1/mogan-research-v1.2.5.1-ubuntu22.04.deb) |
| Mogan Research v1.2.5.1 | Debian 12 | [⏬ Download](http://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/v1.2.5.1/mogan-research-v1.2.5.1-debian12.deb) |

SHA256 checksum:
```
e3713e1506c85dd4ff01b0565f70228486e95ff10ab67379cf0eb1c897c5aa3a  MoganResearch-v1.2.5.1-64bit-installer.exe
f05ef94ac913e7c11a5a3cefa604b5d56c7608b2613b6a7967d64d57a82cf476  MoganResearch-v1.2.5.1-64bit-installer.zip
3cb7d2417345889ad9c8eb66fce52b2168b4189c237a45d02a4c6e3bba19c3d3  MoganResearch-v1.2.5.1-arm.dmg
a2c78e59562924a19d9f744c25efa9d3cc07a105600f2798c40f8994096fe354  MoganResearch-v1.2.5.1.dmg
bb18f3db6de640ef79fa243b394340aaae9ba2f7147bfa48b417e46b1d77cf0a  mogan-research-v1.2.5.1-debian12.deb
b5c84b633c8642522c6ba04c2a8e91dc171e04f8bb922f751a969bd3e8f564ea  mogan-research-v1.2.5.1-ubuntu20.04.deb
a79d7899147a8c8e4cb14b2d77d87208db1ee5f408872ce3f31d9e9e0a4fbb80  mogan-research-v1.2.5.1-ubuntu22.04.deb
```

Mogan STEM Suite:
+ **Mogan Research** (installers on Windows/macOS/Ubuntu/Debian)
+ Mogan Code (no installer, still in development)
+ Mogan Beamer (no installer, still in development)


Other sites to download：
+ https://github.com/XmacsLabs/mogan/releases
+ https://gitee.com/XmacsLabs/mogan/releases
+ https://codeberg.org/XmacsLabs/mogan/releases

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

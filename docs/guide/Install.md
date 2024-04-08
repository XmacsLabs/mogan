# Installation Guide
The latest stable version is Mogan STEM Suite v1.2.5.1 LTS, you can check if there are new releases in the `Help -> Welcome` menu entry.

## Official Packages
| Product | Operating System | Download |
|---------|-------|-----|
| Mogan Research v1.2.5.2 | Windows installer (>=10)| [⏬ Download](https://github.com/XmacsLabs/mogan/releases/download/v1.2.5.2/MoganResearch-v1.2.5.2-64bit-installer.exe) |
| Mogan Research v1.2.5.2 | Windows portable (>=10)| [⏬ Download](https://github.com/XmacsLabs/mogan/releases/download/v1.2.5.2/MoganResearch-v1.2.5.2-64bit-installer.zip) |
| Mogan Research v1.2.5.2 | macOS (>=12) | [⏬ Download](https://github.com/XmacsLabs/mogan/releases/download/v1.2.5.2/MoganResearch-v1.2.5.2.dmg) |
| Mogan Research v1.2.5.2 | macOS arm64 (>=13) | [⏬ Download](https://github.com/XmacsLabs/mogan/releases/download/v1.2.5.2/MoganResearch-v1.2.5.2-arm.dmg) |
| Mogan Research v1.2.5.2 | Ubuntu 20.04 | [⏬ Download](https://github.com/XmacsLabs/mogan/releases/download/v1.2.5.2/mogan-research-v1.2.5.2-ubuntu20.04.deb) |
| Mogan Research v1.2.5.2 | Ubuntu 22.04 | [⏬ Download](https://github.com/XmacsLabs/mogan/releases/download/v1.2.5.2/mogan-research-v1.2.5.2-ubuntu22.04.deb) |
| Mogan Research v1.2.5.2 | Debian 12 | [⏬ Download](http://github.com/XmacsLabs/mogan/v1.2.5.2/mogan-research-v1.2.5.2-debian12.deb) |

SHA256 checksum:
```
3329ad096911dc89374d3648d8243efff9b4242cbd333678b89ebf5e2272887b  MoganResearch-v1.2.5.2-64bit-installer.exe
d7808e16931ae2e00b48ee5ef5987dffbd6a64956a691b84988c930a52066a00  MoganResearch-v1.2.5.2-64bit-installer.zip
25f7241a5e535be29afa0af66bbc1e737ba684fc87518902d7755d4633fd253b  MoganResearch-v1.2.5.2-arm.dmg
eec89670744b1d68e73ef0ca0ca731ca1ecabd99b4dc9d574cdc81b018c443ec  MoganResearch-v1.2.5.2.dmg
2456b8998757528d34bb2cb8998463eb2f7a64ee9cfdd47552b9358c888b98a2  mogan-research-v1.2.5.2-debian12.deb
cb66f18e9915489ec63025fcbc3a275e72f0337ddf15204e3859c34462b74301  mogan-research-v1.2.5.2-ubuntu20.04.deb
62081650da6281b3806416c0493bbb6ef5c9755a1b4f3c3df3101c6d9ca7b582  mogan-research-v1.2.5.2-ubuntu22.04.deb
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

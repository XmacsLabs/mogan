# Installation Guide
The latest stable version is Mogan STEM Suite v1.2.6, you can check if there are new releases in the `Help -> Welcome` menu entry.

## Official Packages
| Product | Operating System | Download |
|---------|-------|-----|
| Mogan Research v1.2.6 | Windows installer (>=10)| [⏬ Download](https://github.com/XmacsLabs/mogan/releases/download/v1.2.6/MoganResearch-v1.2.6-64bit-installer.exe) |
| Mogan Research v1.2.6 | Windows portable (>=10)| [⏬ Download](https://github.com/XmacsLabs/mogan/releases/download/v1.2.6/MoganResearch-v1.2.6-64bit-installer.zip) |
| Mogan Research v1.2.6 | macOS (>=12) | [⏬ Download](https://github.com/XmacsLabs/mogan/releases/download/v1.2.6/MoganResearch-v1.2.6.dmg) |
| Mogan Research v1.2.6 | macOS arm64 (>=13) | [⏬ Download](https://github.com/XmacsLabs/mogan/releases/download/v1.2.6/MoganResearch-v1.2.6-arm.dmg) |
| Mogan Research v1.2.6 | Ubuntu 24.04 | [⏬ Download](https://github.com/XmacsLabs/mogan/releases/download/v1.2.6/mogan-research-v1.2.6-ubuntu24.04.deb) |
| Mogan Research v1.2.6 | Ubuntu 22.04 | [⏬ Download](https://github.com/XmacsLabs/mogan/releases/download/v1.2.6/mogan-research-v1.2.6-ubuntu22.04.deb) |
| Mogan Research v1.2.6 | Debian 12 | [⏬ Download](https://github.com/XmacsLabs/mogan/releases/download/v1.2.6/mogan-research-v1.2.6-debian12.deb) |

SHA256 checksum:
```
13470c2e8484e5dfdc7d2dedc10e30413c8561ba9011ce41896c0cf3966b4652  MoganResearch-v1.2.6-64bit-installer.exe
27265db9fb2523dad86ffc423bee643d46378374b8240dd7c483e80c1ef6de04  MoganResearch-v1.2.6-64bit-installer.zip
a34f8679117d1a18660d10f36fa44d9d86709ab53f57a4cfb73d4b4a0c00cecb  MoganResearch-v1.2.6-arm.dmg
9d2efc2521235bc5166a086c59e803fac4b13d096b9f0449d39d00edfa46d470  MoganResearch-v1.2.6.dmg
7f6abaa4f81bf610ca437c5762845bb0331aed92d07133c7c4025e435931d602  mogan-research-v1.2.6-debian12.deb
3f94a0e7ef8d99b767d58e7cba6dae2593797271a4391a837cf22ee205d45ade  mogan-research-v1.2.6-ubuntu22.04.deb
cfd3ce32c1bdb9efb3f34a5a06fa567dd5624c93357df04290f06bcd7c5d179b  mogan-research-v1.2.6-ubuntu24.04.deb
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
sudo apt install ./mogan-research-v1.2.6-debian12.deb
```
## Ubuntu 20.04/Ubuntu 22.04
Download the official deb and then
```
# For ubuntu 20.04
sudo apt install ./mogan-research-v1.2.6-ubuntu20.04.deb

# For ubuntu 22.04
sudo apt install ./mogan-research-v1.2.6-ubuntu22.04.deb
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

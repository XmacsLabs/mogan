# Mogan v.s. GNU TeXmacs
## Product Positioning
Mogan is a libre STEM suite, consists of Mogan Research, Mogan Code and Mogan Beamer. Mogan Research is one of the distribution of GNU TeXmacs, both of them are libre scientific editing platform.

Mogan Research as a distribution of GNU TeXmacs will try to keep the user experience as the same. Mogan Research and GNU TeXmacs could be regarded as the same kind product released by different teams. Mogan Code and Mogan Beamer will be brand-new products based on the layout engine and structural editing in GNU TeXmacs.

In this article, we will discuss the differences of Mogan Research and GNU TeXmacs in details:

## Preferences
| Option |  Mogan Research | GNU TeXmacs|
|--------|-----------------|------------|
| General`->`Interactive Questions | `In pop-up windows` | `On the footer` |
| Other`->`Document updates run | `Three times` | `Once` |
| Convert`->`Pdf`->`Expand beamer slides | `on` | `off` |

## Shortcuts
### Structured Variants Circling (since v1.2.3)
Add `A-S-up` and `A-S-down` for structured variants circling.

On Linux and Windows, `C-tab` and `C-S-tab` are reserved. On macOS, because we switched `structured:cmd` from `Option` to `Ctrl` and `C-tab` does not work, the shortcuts `C-tab` and `C-S-tab` do not work.

> Why: `C-tab`, `C-S-tab` and `A-S-tab` do not work on macOS, see [QTBUG-12232](https://bugreports.qt.io/browse/QTBUG-12232).

## Graphics
| Action | Mogan Research | GNU TeXmacs |
|--------|------------------|-----------|
| Insert`->`Image`->`Draw image | Show grid | Do not show grid |
| Insert`->`Image`->`Ink here | Show grid | Do not show grid |
| Insert`->`Grid`->`Unit length | 2 | 1 |

## TEXMACS_HOME_PATH
| OS | Mogan Research | GNU TeXmacs |
|---------|-----|-------------|
| Linux   | `$XDG_DATA_HOME/XmacsLabs` | `$HOME/.TeXmacs` |
|         | `$HOME/.local/share/XmacsLabs` |  |
| macOS | `$HOME/Library/Application Support/XmacsLabs` | `$HOME/.TeXmacs` |
| Windows | `%APPDATA%\XmacsLabs` | `%APPDATA%\TeXmacs`|
| | `C:\Users\[User name]\AppData\Roaming\XmacsLabs` | `C:\Users\[User name]\AppData\Roaming\TeXmacs` |

> Why: Using different TEXMACS_HOME_PATH is to make it possible to install both Mogan Research and GNU TeXmacs. The changes on Linux and macOS is to meet the standards on Linux and macOS.

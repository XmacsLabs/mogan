# Mogan v.s. GNU TeXmacs
## Product Positioning
Mogan is a libre STEM suite, consists of Mogan Research, Mogan Code and Mogan Beamer. Mogan Research is one of the distribution of GNU TeXmacs, both of them are libre scientific editing platform.

Mogan Research as a distribution of GNU TeXmacs will try to keep the user experience as the same. Mogan Research and GNU TeXmacs could be regarded as the same kind product released by different teams. Mogan Code and Mogan Beamer will be brand-new products based on the layout engine and structural editing in GNU TeXmacs.

In this article, we will discuss the differences of Mogan Research and GNU TeXmacs in details:

## In one table
| | GNU TeXmacs | Mogan Research |
|--|------------|----------------|
| Release Cycle | almost yearly | monthly |
| UI Style | retro style | modern style | 
| Shortcuts | some of them do not work | 99% of them work fine |
| Scheme Engine | Guile TeXmacs | S7 Scheme |
| Code Repository | SVN (Savannah) | Git (Codeberg/Gitee/Github) |
| Build Tool | GNU Autotools | xmake | 
| Lead by | Joris van der Hoeven (Mathematician) | Darcy Shen (Software Engineer) |

## Preferences
| Option |  Mogan Research | GNU TeXmacs|
|--------|-----------------|------------|
| General`->`Interactive Questions | `In pop-up windows` | `On the footer` |
| Other`->`Document updates run | `Three times` | `Once` |
| Convert`->`Pdf`->`Expand beamer slides | `on` | `off` |

## New Documents
### Enable `Focus->Layout->Margins as on paper` by default
Why:
1. Provide experiences like new document in WPS or MS Word for new TeXmacs user
2. `Insert->Note->Marginal note` works as expected with margins as on paper
3. When `View->Zoom` is 100%, the default page is almost the same with the exported PDF or the printed A4 paper (actually, because there are reserved and hidden 10px on each side on screen, the screen paper is slightly smaller than the actual A4 paper)

### Enable `Focus->Layout->No page numbers` by default
Do not show page numbers to make it clean and tidy.

## Shortcuts
### Structured Key Prefix: `structured:cmd` and `structured:insert`
Structured command key prefix is for toggling the foldable, toggling the numbered, structured moving, structured deleting and structured selecting.

On Windows and Linux, we switched the structured command key prefix from `Alt` to `Ctrl`, the structured insert prefix from `Meta` to `Alt`.

On macOS, we switched the structured command key prefix from `Option` to `Ctrl`, the structured insert prefix from `Ctrl` to `Option`.

> Why：to avoid the collision with the system shortcuts. For example, `Meta+Left` is Windows system shortcuts, `Ctrl+Left` is the macOS system shortcuts。

### Structured Variants Circling (since v1.2.3)
Add `A-S-up` and `A-S-down` for structured variants circling.

On Linux and Windows, `C-tab` and `C-S-tab` are reserved. On macOS, because we switched `structured:cmd` from `Option` to `Ctrl` and `C-tab` does not work, the shortcuts `C-tab` and `C-S-tab` do not work.

> Why: `C-tab`, `C-S-tab` and `A-S-tab` do not work on macOS, see [QTBUG-12232](https://bugreports.qt.io/browse/QTBUG-12232). (__Note__: This argument is __invalid__, as there is no such issue after TeXmacs has upgraded to Qt6 in the latest testing versions.)

### ESC key
The ESC is used for producing equivalents for the modifier keys in GNU TeXmacs. But in Mogan, it is a normal key. And later, the ESC may be used in the Vim plugin for Mogan. If the shortcuts in Mogan is conflicated with the OS system shortucts, we should regard it as a bug and fix it by changing the shortcut but not using the ESC key.

## Graphics
| Action | Mogan Research | GNU TeXmacs |
|--------|------------------|-----------|
| Insert`->`Image`->`Draw image | Show grid | Do not show grid |
| Insert`->`Image`->`Ink here | Show grid | Do not show grid |
| Insert`->`Grid`->`Unit length | 2 | 1 |

## Plugins
Most plugins in GNU TeXmacs are no longer built-in in Mogan Research. Only the Maxima Session plugin and the Octave Session plugin are built-in in Mogan Research.

The concepts of plugins are extended in Mogan Research, see [Introduction to Plugins](plugins.md), for example, natural languages specific dictionaries and styles are maintained as Language plugins.

And Plugin Center will be available later for Mogan users to install the community plugins.


## TEXMACS_HOME_PATH
| OS | Mogan Research | GNU TeXmacs |
|---------|-----|-------------|
| Linux   | `$XDG_DATA_HOME/XmacsLabs` | `$HOME/.TeXmacs` |
|         | `$HOME/.local/share/XmacsLabs` |  |
| macOS | `$HOME/Library/Application Support/XmacsLabs` | `$HOME/.TeXmacs` |
| Windows | `%APPDATA%\XmacsLabs` | `%APPDATA%\TeXmacs`|
| | `C:\Users\[User name]\AppData\Roaming\XmacsLabs` | `C:\Users\[User name]\AppData\Roaming\TeXmacs` |

> Why: Using different TEXMACS_HOME_PATH is to make it possible to install both Mogan Research and GNU TeXmacs. The changes on Linux and macOS is to meet the standards on Linux and macOS.

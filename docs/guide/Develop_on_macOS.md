# Developing on macOS
This guide is for Mogan Research. For other components, the steps are similar to this one.

## Step 1: Install xmake and xrepo
For Homebrew:
```
brew install xmake qt
brew install pkg-config
```

Sometimes, we need the latest xrepo:
``` bash
xrepo update-repo
```

## Step 2: Config
```
xmake config --yes -c --qt=/opt/homebrew/share/qt/
```
The Qt Dir might not be `/opt/homebrew/share/qt`, please adjust it if needed.

## Step 3: Build
``` bash
xmake build research
```

## Step 4: Run tests
See [How to test](Test.md)。

## Step 5: Launch Mogan Research
``` bash
xmake run research
```

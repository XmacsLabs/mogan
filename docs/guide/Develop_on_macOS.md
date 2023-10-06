# Developing on macOS
## Using xmake
### Step 1: Install xmake and xrepo
For Homebrew:
```
brew install xmake qt
brew install pkg-config
```

Sometimes, we need the latest xrepo:
``` bash
xrepo update-repo
```

Sometimes we need to use the newest version xmake to build. It is a problem in xmake 2.8.3, and it should not happen in the future version of xmake.

You can use
```
xmake update -s dev
```
to update xmake version.

### Step 2: Config
```
xmake config --qt=/opt/homebrew/share/qt/ --yes
```
The Qt Dir might not be `/opt/homebrew/share/qt`, please adjust it if needed.

### Step 3: Build
``` bash
xmake build research
```

### Step 4: Run tests
See [How to test](Test.md)。

### Step 5: Launch Mogan Research
``` bash
xmake run research
```

# Developing on macOS
## Using xmake
### Step 1: Install xmake and xrepo
For Homebrew:
```
brew install xmake qt@5 ccache
brew link qt@5
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

### Step 2: Compile
``` bash
xmake config --yes
xmake build research
```

### Step 3: Run tests
See [How to test](Test.md)。

### Step 4: Install to `build/macosx/{arch}/release/Mogan.app/`
``` bash
xmake install research
```

### Step 5: Launch Mogan Editor
``` bash
xmake run research
```

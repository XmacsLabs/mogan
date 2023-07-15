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

### Step 2: Compile
``` bash
xmake config --yes
xmake build --yes --verbose --all
```

### Step 3: Run tests
See [How to test](Test.md)。

### Step 4: Install to `build/macosx/{arch}/release/Mogan.app/`
``` bash
xmake install mogan
```

### Step 5: Launch Mogan Editor
``` bash
xmake run mogan
```

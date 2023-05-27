# Developing on macOS
## Using xmake
### Step 1: Install xmake and xrepo
For Homebrew:
```
brew install xmake qt@5 ccache
brew link qt@5
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

### Step 3: Run unit test
``` bash
xmake run --yes --verbose --diagnosis --group=tests
```

### Step 4: Install to `build/macosx/{arch}/release/Mogan.app/`

``` bash
xmake install -o build/macosx/`arch`/release/Mogan.app/Contents/Resources/ mogan_install
```

### Step 5: Launch Mogan Editor
Sometimes (for macOS M1), we need to codesign it first
``` bash
codesign --force --deep --sign - ./build/macosx/arm64/release/Mogan.app
```

``` bash
./build/macosx/`arch`/release/Mogan.app/Contents/MacOS/Mogan
```

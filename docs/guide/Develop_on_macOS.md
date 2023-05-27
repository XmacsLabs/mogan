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

## Using cmake
Assuming you are in the project root of mogan:
``` bash
mkdir build && cd build
cmake .. -DCMAKE_INSTALL_PREFIX=$HOME/software
make install -j12
```

Now, you can launch Mogan via:
``` bash
$HOME/software/bin/mogan.sh
```

Using `make install` and developing in a separated folder could help you setup a clean environment. If you are familiar with the source code, you can change the TEXMACS_PATH to develop on the Scheme part without having to build the C++ part.

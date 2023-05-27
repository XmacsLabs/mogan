# Developing on GNU/Linux
## Using xmake
### Step 1: Install xmake and Qt 5
On Debian or Debian derivatives:
```
sudo add-apt-repository ppa:xmake-io/xmake
sudo apt update
sudo apt upgrade --yes gcc
sudo apt install --yes qtbase5-dev libqt5svg5-dev xmake
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
TEXMACS_PATH=$PWD/TeXmacs xmake run --yes --verbose --diagnosis --group=tests
```

### Step 4: Install to `build/package`
``` bash
xmake install -o build/package mogan_install
```

### Step 5: Launch Mogan Editor
``` bash
build/package/bin/mogan
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


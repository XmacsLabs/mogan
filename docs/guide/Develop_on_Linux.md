# Developing on GNU/Linux
## Using xmake
### Step 1: Install xmake and Qt 5
On Debian or Debian derivatives:
```
sudo add-apt-repository ppa:xmake-io/xmake
sudo apt install xmake

sudo apt update
sudo apt install --yes build-essential libfontconfig1-dev qtbase5-dev libqt5svg5-dev
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

If Qt SDK is not found, we can config it manually:
```
xmake config --qt=/usr/lib/`arch`-linux-gnu/qt5/
```
To switch to Qt 6, just:
```
xmake config --qt=/usr/lib/`arch`-linux-gnu/qt6/
```

### Step 3: Run tests
See [How to test](Test.md)。

### Step 4: Install to `build/package`
``` bash
xmake install -o build/package mogan_install
```

### Step 5: Launch Mogan Editor
``` bash
build/package/bin/mogan
```

### Use VSCode to help code completion
Install VSCode, Clangd and the Clangd plugin for VSCode.
Then execute in the mogan folder
````
xmake project -k compile command
````
This will generate a compile_commands.json file under the mogan folder, and Clangd will read it to understand the organizational structure of the project, so as to avoid the error that the header file cannot be found.
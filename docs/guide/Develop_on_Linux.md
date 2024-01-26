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

On Fedora and RHEL derivatives:
```
sudo dnf update
sudo dnf groupinstall "C Development Tools and Libraries"
sudo dnf install git wget unzip glibc-static libstdc++-static qt5-qtbase-devel qt5-qtsvg-devel fontconfig-devel
wget https://xmake.io/shget.text -O - | bash #install xmake
```

Sometimes, we need the latest xrepo:
``` bash
xrepo update-repo
```

### Step 2: Compile
``` bash
xmake config --yes
xmake build research
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

### Step 4: Install to `build/packages/app.mogan`
``` bash
xmake install research
```

### Step 5: Launch Mogan Research
``` bash
xmake run research
```

### Use VSCode to help code completion
Install VSCode, Clangd and the Clangd plugin for VSCode.
Then execute in the mogan folder
````
xmake project -k compile_commands
````
This will generate a compile_commands.json file under the mogan folder, and Clangd will read it to understand the organizational structure of the project, so as to avoid the error that the header file cannot be found.

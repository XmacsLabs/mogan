# Python3 Binary plugin
+ Executable Filename: python3
+ Plugin Source Code:
  - [Codeberg](https://codeberg.org/XmacsLabs/mogan/src/branch/branch-1.2/TeXmacs/plugins/binary/progs/binary/python3.scm)
  - [Gitee](https://gitee.com/XmacsLabs/mogan/blob/branch-1.2/TeXmacs/plugins/binary/progs/binary/python3.scm)
  - [Github](https://github.com/XmacsLabs/mogan/blob/branch-1.2/TeXmacs/plugins/binary/progs/binary/python3.scm)

## How to Install Python 3

### Windows
Go to the [Python official website](https://www.python.org) to download and install the package.

If the user did not install Python in the default path, they need to manually add the directory where `python3.exe` is located to the PATH.

### macOS/Linux
Python is included with the system and does not require installation.

## How to Customize the Python Interpreter
You can use a custom Python interpreter by customizing the Python 3 binary plugin.

The key is to replace the implementation of `(find-binary-python3)`:

```scheme
(tm-define (find-binary-python3)
  (system->url "/path/to/your/python3/interpreter"))
```

For more information on how to customize the Python3 binary plugin, please refer to the documentation: [Binary Plugins](plugin_binary.md).
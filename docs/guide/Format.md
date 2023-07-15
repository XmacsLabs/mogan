# How to format the code
```
elvish bin/format
```

## clang-format
Pin clang-format to `15.0.7`, because the default version of `clang-format` on ubuntu is `15.0.7`.

### Windows
```
scoop install llvm@15.0.7
```

### Linux
We suggest that you should use Ubuntu 22.04.
```
sudo apt install clang-format-15
```
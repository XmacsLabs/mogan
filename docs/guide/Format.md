# How to format the code
```
elvish bin/format
```
## clang-format
Pin clang-format to `16.0.x`.

### macOS
```
brew install clang-format
```
### Windows
```
scoop install llvm@16.0.6
```

### Linux
We need to install llvm manually on Linux.

For example:
```
xrepo install "llvm==16.0.5"
```

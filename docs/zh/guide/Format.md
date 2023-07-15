# 如何格式化代码
```
elvish bin/format
```

## clang-format
将`clang-format`固定为`15.0.7`，因为Ubuntu 22.04里面内置的`clang-format`是`15.0.7`。

### Linux
推荐使用Ubuntu 22.04开发，这样可以直接使用apt安装
```
sudo apt install clang-format-15
```

### Windows
```
scoop install llvm@15.0.7
```

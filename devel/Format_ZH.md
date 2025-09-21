# 格式化

## 环境配置
需要19.1.x版本的clang-format，安装方式：

### Linux
首先，通过apt安装
```
sudo apt install clang-format-19
```
重启终端后检查版本是否为19
```
clang-format-19 --version
```

### macOS
> 注意：这个应该需要对应改为LLVM 19

首先，通过homebrew安装
```
brew install llvm@19
```
然后在.zshrc中添加
```
# set clang-format to version 19
export PATH="$(brew --prefix llvm@19)/bin:$PATH"
```
重启终端后检查版本是否为19
```
clang-format --version
```

### Windows

首先通过scoop安装
```
scoop install llvm@19.1.0
```
重启终端后检查版本是否为19
```
clang-format --version
```

## 使用脚本
进入git仓库，使用脚本命令格式化：
```
elvish bin/format
```

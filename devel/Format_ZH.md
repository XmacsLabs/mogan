# 格式化

## 环境配置
需要16.0.x版本的clang-format,安装方式：
### Linux
首先，通过apt安装
```
sudo apt install clang-format-19
```
重启终端后检查版本是否为19
```
clang-format --version
```

### macOS
> 注意：这个应该需要对应改为LLVM 19

首先，通过homebrew安装
```
brew install llvm@16
```
然后在.zshrc中添加
```
# set clang-format to version 16
export PATH="$(brew --prefix llvm@16)/bin:$PATH"
```
重启终端后检查版本是否为16
```
clang-format --version
```

### Windows
> 注意：这里需要对应改为 LLVM 19

首先通过scoop安装
```
scoop install llvm@16.0.6
```
重启终端后检查版本是否为16
```
clang-format --version
```

## 使用脚本
进入git仓库，使用脚本命令格式化：
```
elvish bin/format
```

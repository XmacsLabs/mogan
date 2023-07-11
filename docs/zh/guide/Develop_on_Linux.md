# 在GNU/Linux上开发

## 使用xmake
### 第一步：安装xmake和Qt 5
以Debian及衍生版为例：
```
sudo add-apt-repository ppa:xmake-io/xmake
sudo apt install xmake

sudo apt update
sudo apt install --yes build-essential libfontconfig1-dev qtbase5-dev libqt5svg5-dev
```

有时候需要保持xrepo是最新的，以保证使用最新的依赖的构建定义
```
xrepo update-repo
```

### 第二步：编译
```
xmake config --yes
xmake build --yes --verbose --all
```

如果找不到Qt，那么在config的时候，可以手动指定，比如：
```
xmake config --qt=/usr/lib/`arch`-linux-gnu/qt5/
```
切换到Qt 6，也只需要：
```
xmake config --qt=/usr/lib/`arch`-linux-gnu/qt6/
```

### 第三步：测试
见[如何测试](Test.md)。

### 第四步: 安装到`build/packages/app.mogan`
``` bash
xmake install mogan
```

### 第五步：启动墨干编辑器
``` bash
xmake run mogan
```

### 使用VSCode帮助代码补全
安装VSCode，Clangd以及VSCode的Clangd插件。
之后在mogan文件夹执行
```
xmake project -k compile_commands
```
这个命令会在mogan文件夹下生成一个compile_commands.json文件，Clangd会读取它以理解项目的组织结构，从而避免找不到头文件的错误。
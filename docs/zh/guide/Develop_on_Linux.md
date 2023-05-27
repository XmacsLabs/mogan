# 在GNU/Linux上开发

## 使用xmake
### 第一步：安装xmake和Qt 5
以Debian及衍生版为例：
```
sudo add-apt-repository ppa:xmake-io/xmake
sudo apt update
sudo apt upgrade --yes gcc
sudo apt install --yes qtbase5-dev libqt5svg5-dev xmake
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

### 第三步：运行单元测试
``` bash
TEXMACS_PATH=$PWD/TeXmacs xmake run --yes --verbose --diagnosis --group=tests
```

### 第四步: 安装到`build/package`
``` bash
xmake install -o build/package mogan_install
```

### 第五步：启动墨干编辑器
``` bash
build/package/bin/mogan
```

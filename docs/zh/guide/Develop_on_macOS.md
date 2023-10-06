# 在macOS上开发

## 使用xmake
### 第一步：安装xmake和xrepo
对于Homebrew:
```
brew install xmake qt
brew install pkg-config
```

有时候需要保持xrepo是最新的，以保证使用最新的依赖的构建定义。
```
xrepo update-repo
```

有时需要xmake使用最新的版本才能正常编译，在xmake 2.8.3中存在这个问题，后面的版本应该不会有这个问题。

可以用
```
xmake update -s dev
```
来切换到最新版本

### 第二步：配置
```
xmake config --qt=/opt/homebrew/share/qt/ --yes
```
请按需调整Qt相关目录，并不一定是`/opt/homebrew/share/qt`。

### 第三步：构建
```
xmake build research
```

### 第四步：测试
见[如何测试](Test.md)。

### 第五步：启动墨砚
``` bash
xmake run research
```

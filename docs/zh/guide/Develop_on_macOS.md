# 在macOS上开发

## 使用xmake
### 第一步：安装xmake和xrepo
对于Homebrew:
```
brew install xmake qt@5 ccache
brew link qt@5
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

### 第三步：测试
见[如何测试](Test.md)。

### 第四步: 安装到`build/macosx/{arch}/release/Mogan.app/`
``` bash
xmake install -o build/macosx/`arch`/release/Mogan.app/Contents/Resources/ mogan_install
```

### 第五步：启动墨干编辑器
``` bash
./build/macosx/`arch`/release/Mogan.app/Contents/MacOS/Mogan

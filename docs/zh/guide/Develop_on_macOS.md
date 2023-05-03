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
xmake
```

### 第三步：运行单元测试
``` bash
TEXMACS_PATH=$PWD/TeXmacs xmake run --yes --verbose --diagnosis --group=tests
```

### 第四步: 安装到`build/macosx/{arch}/release/Mogan.app/`

``` bash
# x86_64
xmake install -o build/macosx/x86_64/release/Mogan.app/Contents/Resources/ mogan_install

# arm64
xmake install -o build/macosx/arm64/release/Mogan.app/Contents/Resources/ mogan_install
```


### 第五步：启动墨干编辑器
对于需要Codesign的macOS版本（比如macOS M1），需要先签名：
``` bash
codesign --force --deep --sign - ./build/macosx/arm64/release/Mogan.app
```

然后再启动：
``` bash
# x86_64
./build/macosx/x86_64/release/Mogan.app/Contents/MacOS/Mogan

# arm64
./build/macosx/arm64/release/Mogan.app/Contents/MacOS/Mogan
```

## 使用cmake
假定你已经在mogan的源代码目录：
``` bash
mkdir build && cd build
cmake .. -DCMAKE_INSTALL_PREFIX=$HOME/software
make install -j12
```

编译安装完成之后，可以这样启动墨干：
``` bash
$HOME/software/bin/mogan.sh
```

使用`make install`，然后在单独的目录里面做开发，能够排除一些干扰。如果你对源代码比较了解，可以通过更改TEXMACS_PATH的方式，直接在源代码中更改Scheme这部分代码，不需要重新编译就可以测试你的更改。

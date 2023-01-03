# 开发者文档
## Using xmake on Windows
墨干的代码使用变长数组，因此不能用msvc来编译开发。推荐使用mingw编译器来开发。

### 第一步：安装xmake、mingw、msys以及Qt
开发者需要安装下列工具：

* [Qt](https://www.qt.io/download)
    * 只需要安装适配mingw 8.1.0的qt库。Qt5和Qt6都可以安装。不需要安装Qt script等其他框架。
    * 推荐安装Qt提供的mingw 8.1.0编译器，这个编译器在tools子菜单下。
* [msys environment](https://github.com/msys2/msys2-installer/releases)
* mingw 8.1.0编译器（见下文）。
* xmake（见下文）。

既可以用msys2的pacman安装xmake，也可以用xmake的安装包来安装。

**注意**: msys提供的Mingw版本很高，与墨干的代码不兼容。请通过qt安装程序或者chocolate安装8.1.0版本的Mingw。

有时，开发者要更新xmake的包数据：
``` pwsh
xrepo update-repo
```

### 第二步：编译
在msys命令行下运行这两条命令
``` bash
xmake config --qt=<newly installed qt address>/5.x.x/mingw81_64
xmake build --jobs=<numbers of processes your computer can support, same as make>
```

### 第三步：运行单元测试
``` bash
windeployqt --compiler-runtime ./build/mingw/x86_64/release/ -printsupport
xmake run --yes --verbose --diagnosis --group=tests
xmake run --yes --verbose --diagnosis --group=keneral_tests
```

### 第四步：安装到`build/package`目录
``` bash
xmake install -o build/package mogan_install
windeployqt --compiler-runtime ./build/package/bin/ -printsupport
```

墨干编辑器的某些功能需要其他程序来配合。这些程序在[xmacslabs/mogan-dependencies](https://github.com/XmacsLabs/mogan-dependencies)仓库下。

### 第五步：启动墨干编辑器
``` bash
build/package/bin/mogan.exe
```

### 在VSCode下开发（可选）
如果用VSCode来开发，xmake可以生成`compile_command.json`文件。如果提供这份文件，VSCode的C++插件会根据这个文件来识别项目结构，提供正确的语义高亮等语义提示。
```bash
xmake project --kind=compile_commands ./.vscode
```

修改`.vscode/c_cpp_properties.json`文件才能使刚刚生成的`compile_command.json`文件生效。
```jsonc
{
    {
    "configurations": [
        {
            // other configs
            "compilerPath": "<your configuration>",
            "cppStandard": "gnu++17",
            "intelliSenseMode": "windows-gcc-x64",
            "compileCommands": ".vscode/compile_commands.json"
        }
    ],
}
}
```

##  在Linux或WSL环境下使用CMake开发
> 注意: 由于移除了nowide的源代码而依赖于nowide的库，使用cmake和mxe可能会比较麻烦，我们推荐使用xmake。

我们需要在Windows上使用WSL里面的Ubuntu 20.04或者在Ubuntu 20.04上启动Windows的虚拟机。因为构建是在Ubuntu上面完成，而测试是在Windows上完成。

首先，我们需要完成MXE的下载和相关依赖的安装，可以参考这个文档：
https://texmacs.github.io/notes/docs/build-using-cmake-and-mxe-on-wsl.html

假定你已经在mogan的源代码目录：
```
rm -rf build/
./packages/windows/package.sh
```
安装完成之后，我们需要通过虚拟机和宿主机之间的文件共享，在Windows系统中启动墨干，并做测试。

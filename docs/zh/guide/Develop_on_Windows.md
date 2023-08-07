# 开发者文档
墨干的代码使用变长数组，因此不能用msvc来编译开发。推荐使用MinGW编译器来开发。

### 第一步：安装xmake、MinGW、msys以及Qt
开发者需要安装下列工具：

* [Qt](https://www.qt.io/download)
    * 只需要安装适配MinGW 8.1.0的Qt库。Qt 5 和Qt 6 都可以安装（Qt 5.15.2是一个很好的选择,可以适配MinGW 8.1.0）。不需要安装Qt script等其他框架。
* [msys environment](https://github.com/msys2/msys2-installer/releases)
* MinGW 8.1.0编译器（见下文）。
    * 推荐安装Qt提供的MinGW 8.1.0编译器，这个编译器在Tools子目录下。
* xmake（见下文）。

既可以用msys2的pacman安装xmake，也可以用xmake的安装包来安装。

建议使用pacman, 同时还需要安装其他的包：

```
pacman -Sy xmake
pacman -Sy make
pacman -Sy git
pacman -Sy mingw-w64-x86_64-7zip
```

**注意**: msys提供的MinGW版本很高，与墨干的代码不兼容。请通过Qt安装程序或者chocolate安装8.1.0版本的MinGW。

有时，开发者要更新xmake的包数据：
``` pwsh
xrepo update-repo
```

### 第二步：编译
在msys命令行下运行这两条命令。

```
xmake config --yes --verbose --diagnosis --plat=mingw --mingw=<newly installed qt address>/Tools/mingw810_64 --qt=<newly installed qt address>/5.x.x/mingw81_64
```

上面的命令行会选择Qt安装程序安装的MinGW 8.1.0。

``` bash
xmake build research
```

### 第三步：运行单元测试

运行命令`windeployqt`前需要将`<newly installed qt address>/5.15.2/mingw81_64/bin`添加到`PATH`环境变量。

``` bash
windeployqt --compiler-runtime ./build/mingw/x86_64/release/ -printsupport
xmake run --yes --verbose --diagnosis --group=tests
xmake run --yes --verbose --diagnosis --group=keneral_tests
```

### 第四步：安装到`build/packages`目录
``` bash
xmake install research
```

墨干编辑器的某些功能需要其他程序来配合。这些程序在[xmacslabs/mogan-dependencies](https://github.com/XmacsLabs/mogan-dependencies)仓库下。

### 第五步：启动墨干编辑器
``` bash
xmake run research
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

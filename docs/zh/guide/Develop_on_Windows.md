# Windows平台开发指南
## 第一步：安装xmake
首先安装Scoop，然后用Scoop安装xmake：
```
scoop install xmake
```

## 第二步：xmake config
第一次运行`xmake config`的时候，可能需要启动有管理员权限的Windows终端，因为libgit2需要这个权限。
```
xmake config --yes -vD -m releasedbg --plat=windows
```
后面由于已经缓存了libgit2，直接使用普通的Windows终端就可以了。

## 第三步：编译
``` bash
xmake build research
```
## 第四步：测试
见[如何测试](Test.md)。

## 第五步：启动墨干
``` bash
xmake i research
xmake r research
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

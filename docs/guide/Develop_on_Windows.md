# Developing for Windows
## Step 1: Install xmake
Install scoop and then use scoop to install xmake
```
scoop install xmake
```

### Step 2: Config
Because libgit2 requires admin console, you need to launch a admin console to execute the following command:
```
xmake config --yes -vD -m releasedbg --plat=windows
```
The next you run `xmake config`, it is ok to run without the admin console.

## Step 3: Build
``` bash
xmake build research
```

## Step 4: Run tests
See [How to test](Test.md)。

## Step 5: Launch Mogan Research
``` bash
xmake i research
xmake r research
```

## Optional: Vscode support
Developers using Vscode can use xmake to generate `compile_command.json`, which can be recognized by C/C++ plugin to provide semantics highlight and so on.
```bash
xmake project --kind=compile_commands ./.vscode
```

then modify `.vscode/c_cpp_properties.json` to tell C++ lsp configs of this project:
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

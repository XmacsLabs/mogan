# macOS平台开发指南
本开发指南针对墨干理工套件中的墨干（Mogan Research）撰写，墨干理工套件中的其它组件可以参考。

## 第一步：安装xmake和xrepo
对于Homebrew:
```
brew install xmake qt
brew install pkg-config
```

有时候需要保持xrepo是最新的，以保证使用最新的依赖的构建定义。
```
xrepo update-repo
```

## 第二步：配置
```
xmake config -vD --yes 
```
如果找不到Qt，可以手动指定，例如：
```
xmake config --qt=/opt/homebrew/share/qt
```
请按需调整Qt相关目录，并不一定是`/opt/homebrew/share/qt`。

## 第三步：构建
```
xmake build stem
```

## 第四步：测试
见[如何测试](Test_ZH.md)。

## 第五步：启动墨干
``` bash
xmake run stem
```
## commit前请格式化
[格式化链接](https://gitee.com/XmacsLabs/mogan/blob/main/devel/Format_ZH.md)
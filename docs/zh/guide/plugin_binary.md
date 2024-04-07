# 二进制插件
二进制插件是一类插件，主要用于：
+ 定位可执行文件的位置并判断可执行文件是否存在
+ 依据实际需求调用可执行文件，并封装输入的预处理和输出的后置处理逻辑

## 共享和独享
共享的二进制插件主要在`$TEXMACS_PATH/plugins/binary`里面，一个可执行文件对应一个源代码文件：
```
$TEXMACS_PATH/plugins/binary/progs/binary/<name>.scm
```
比如Ghostscript的gs命令对应的源代码在
```
$TEXMACS_PATH/plugins/binary/progs/binary/gs.scm
```

独享的二进制插件在
```
$TEXMACS_PATH/plugins/<name>/progs/binary/<name>.scm
```
比如Octave插件的可执行文件对应的源代码在
```
$TEXMACS_PATH/plugins/octave/progs/binary/octave.scm
```

## 三要素
| 函数  | 用途  |
|---|---|
| `(find-binary-xyz)` | 定位xyz可执行文件的位置，如果不存在，则返回`(url-none)` |
| `(has-binary-xyz?)` | 判断xyz可执行文件是否存在，如果存在，返回`#t`，如果不存在，返回`#f` |
| `(version-binary-xyz?)` | 返回xyz可执行文件的版本，如果该可执行文件不存在，返回空字符串，如果存在，则返回包含版本信息的字符串 |

## `(find-binary-xyz)`详解
+ 如果配置项`plugin:binary`为`off`，则`(find-binary-xyz)`返回`(url-none)`
  + 如果配置项`plugin:binary:xyz`不是`default`，则检测该配置项中的路径是否存在，如果存在，就返回xyz可执行文件的路径
    + 如果xyz.scm定义的候选路径`(xyz-binary-candidates)`存在，则返回该候选路径
      + 最终，直接在系统路径中查找xyz可执行文件

二进制插件作者只需要关注`(xyz-binary-candidates)`的实现是否合理即可。配置项`plugin:binary`目前（1.2.5.2）只支持使用Scheme代码`(set-preference "plugin:binary" "/path/to/binary/xyz")`配置。

## 帮助`->`插件`->`二进制
在墨干中点击`帮助->插件->二进制`，可以找到三段可运行的Scheme代码片段：
1. 如何定位某一个二进制插件的路径
2. 如何自定义某一个二进制插件的路径
3. 如何禁用所有的二进制插件

# 二进制插件
二进制插件的源代码在`$TEXMACS_PATH/plugins/binary`里面，一个可执行文件对应一个源代码文件：
```
$TEXMACS_PATH/plugins/binary/progs/binary/<name>.scm
```
比如Ghostscript的gs命令对应的源代码在
```
$TEXMACS_PATH/plugins/binary/progs/binary/gs.scm
```
如果用户需要修改寻找gs命令行或者调用gs命令行的Scheme函数，直接将上述`gs.scm`拷贝到：
```
$TEXMACS_HOME_PATH/plugins/binary/progs/binary/gs.scm
```
然后做相关修改即可。`$TEXMACS_HOME_PATH`下面的`gs.scm`（如果存在的话）会优先于`$TEXMACS_PATH`下面`gs.scm`被加载。

# 插件
墨干的插件按照是否内置，可分为内置插件和社区插件两种。按照功能分为：
+ **二进制插件**: 提供定位和使用可执行文件的Scheme函数
+ **图像插件**：为不同格式的图像提供格式转换，比如PDF图像插件
+ **语言插件**：为不同的自然语言提供字典和样式定制，比如中文插件
+ **数据格式插件**：处理数据格式转换的插件，比如HTML数据插件、LaTeX数据插件
+ **代码插件**：提供编程语言的代码高亮定义和相应的样式、编辑器定制，比如Python代码插件
+ **会话插件**：提供外部系统的集成，且带有上下文，比如Maxima插件

## 二进制插件
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

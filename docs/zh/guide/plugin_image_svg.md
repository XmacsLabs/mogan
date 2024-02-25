# SVG图像插件
SVG图像插件定义了从SVG格式到PNG、Postscript、PDF格式的转换器。
+ SVG2PNG: 用于编辑器中图像的实时渲染
+ SVG2EPS: 用于将TeXmacs文档导出为Postscript文档
+ SVG2PDF: 用户将TeXmacs文档导出为PDF文档

SVG2PNG目前是通过Qt的SVG支持实现的，无需依赖外部软件，对于墨干V1.2.5而言，无法配置转换方式。

从SVG到EPS和PDF的格式转换如果需要保留矢量图无限制缩放的特性，那么需要用户手动配置[rsvg-convert二进制插件](plugin_binary_rsvg_convert.md)或者[Inkscape二进制插件](plugin_binary_inkscape.md)。具体规则如下：
+ 优先使用rsvg-convert或Inkscape；在rsvg-convert和Inkscape都可用的情况下，会优先使用Inkscape做转换。
+ 如果两者都不可用，则使用ImagicMagick的convert命令做相关转换。
+ 如果所有命令行都不可用，那么导出的PDF或者Postscript文档中的图像变为位图或者会弹出错误提示。

SVG图像插件是单文件插件，源代码如下所示：
+ https://codeberg.org/XmacsLabs/mogan/src/branch/branch-1.2/TeXmacs/plugins/image/progs/image/svg.scm
+ https://gitee.com/XmacsLabs/mogan/blob/branch-1.2/TeXmacs/plugins/image/progs/image/svg.scm
+ https://github.com/XmacsLabs/mogan/blob/branch-1.2/TeXmacs/plugins/image/progs/image/svg.scm 

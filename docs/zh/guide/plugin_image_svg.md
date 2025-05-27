# SVG 图像插件

SVG 图像插件定义了从 SVG 格式到 PNG、Postscript、PDF 格式的转换器。

- SVG2PNG: 用于编辑器中图像的实时渲染
- SVG2EPS: 用于将 TeXmacs 文档导出为 Postscript 文档
- SVG2PDF: 用户将 TeXmacs 文档导出为 PDF 文档

SVG2PNG 目前是通过 Qt 的 SVG 支持实现的，无需依赖外部软件，对于墨干 V1.2.5 而言，无法配置转换方式。

从 SVG 到 EPS 和 PDF 的格式转换如果需要保留矢量图无限制缩放的特性，那么需要用户手动配置[rsvg-convert 二进制插件](plugin_binary_rsvg_convert.md)或者[Inkscape 二进制插件](plugin_binary_inkscape.md)。具体规则如下：

- 优先使用 rsvg-convert 或 Inkscape；在 rsvg-convert 和 Inkscape 都可用的情况下，会优先使用 Inkscape 做转换。
- 如果两者都不可用，则使用 ImagicMagick 的 convert 命令做相关转换。
- 如果所有命令行都不可用，那么导出的 PDF 或者 Postscript 文档中的图像变为位图或者会弹出错误提示。

SVG 图像插件是单文件插件，源代码如下所示：

- https://codeberg.org/XmacsLabs/mogan/src/branch/branch-1.2/TeXmacs/plugins/image/progs/image/svg.scm
- https://gitee.com/XmacsLabs/mogan/blob/branch-1.2/TeXmacs/plugins/image/progs/image/svg.scm
- https://github.com/XmacsLabs/mogan/blob/branch-1.2/TeXmacs/plugins/image/progs/image/svg.scm

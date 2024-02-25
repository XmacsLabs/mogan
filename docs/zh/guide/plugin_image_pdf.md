# PDF图像插件
SVG图像插件定义了从SVG格式到PNG、Postscript、PDF格式的转换器。
+ SVG2PNG: 用于编辑器中图像的实时渲染
+ SVG2EPS: 用于将TeXmacs文档导出为Postscript文档
+ SVG2PDF: 用户将TeXmacs文档导出为PDF文档

墨干的渲染器无法支持PDF图像，通常此类PDF图像是PDF矢量图，由第三方软件制作（比如在Octave或者Maxima导出为PDF矢量图）。

墨干在渲染PDF图像时，需要依赖于Ghostscipt将PDF图像转换为PNG图像。故而该插件需要用户手动安装Ghostscript（见[Ghostscript二进制插件](plugin_binary_gs.md)）。

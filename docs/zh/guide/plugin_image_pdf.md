# PDF 图像插件

SVG 图像插件定义了从 SVG 格式到 PNG、Postscript、PDF 格式的转换器。

- SVG2PNG: 用于编辑器中图像的实时渲染
- SVG2EPS: 用于将 TeXmacs 文档导出为 Postscript 文档
- SVG2PDF: 用户将 TeXmacs 文档导出为 PDF 文档

墨干的渲染器无法支持 PDF 图像，通常此类 PDF 图像是 PDF 矢量图，由第三方软件制作（比如在 Octave 或者 Maxima 导出为 PDF 矢量图）。

墨干在渲染 PDF 图像时，需要依赖于 Ghostscipt 将 PDF 图像转换为 PNG 图像。故而该插件需要用户手动安装 Ghostscript（见[Ghostscript 二进制插件](plugin_binary_gs.md)）。

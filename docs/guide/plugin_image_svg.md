# SVG Image Plugin
The SVG Image Plugin defines converters from SVG format to PNG, Postscript, and PDF formats.

+ SVG2PNG: Used for real-time rendering of images in the editor.
+ SVG2EPS: Used for exporting TeXmacs documents to Postscript format.
+ SVG2PDF: Used for exporting TeXmacs documents to PDF format.

SVG2PNG is currently implemented through Qt's SVG support, without the need for external software dependencies. For Mogan v1.2.5, the conversion method cannot be configured.

To convert from SVG to EPS and PDF formats while preserving the unlimited scaling characteristics of vector graphics, users need to manually configure the [rsvg-convert binary plugin](plugin_binary_rsvg_convert.md) or the [Inkscape binary plugin](plugin_binary_inkscape.md). The specific rules are as follows:

+ Priority is given to rsvg-convert or Inkscape. If both are available, Inkscape will be preferred for conversion.
+ If both are unavailable, the convert command of ImagicMagick will be used for the conversion.
+ If all command lines are unavailable, the images in the exported PDF or Postscript documents will become bitmaps or an error message will be displayed.

The SVG Image Plugin is a single-file plugin, and its source code is available at the following locations:

+ https://codeberg.org/XmacsLabs/mogan/src/branch/branch-1.2/TeXmacs/plugins/image/progs/image/svg.scm
+ https://gitee.com/XmacsLabs/mogan/blob/branch-1.2/TeXmacs/plugins/image/progs/image/svg.scm
+ https://github.com/XmacsLabs/mogan/blob/branch-1.2/TeXmacs/plugins/image/progs/image/svg.scm

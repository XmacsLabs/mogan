# DOCX 数据插件
## TeXmacs -> DOCX
在 Mogan Research 中，你可以通过以下步骤将文档导出为 DOCX 格式：`文件 -> 导出 -> DOCX`。

### 前置必要条件：`Pandoc`
TeXmacs 文档转换为 DOCX 文档的过程依赖于第三方应用 `Pandoc`。如果在导出时没有正确安装并配置 `Pandoc`，则会出现错误，导致无法导出 DOCX 文件。

#### 为什么需要 Pandoc？
Pandoc 是一个通用的文件格式转换工具，它能够将 TeXmacs 文档转换为 DOCX 格式。这是因为 TeXmacs 本身并不直接支持 DOCX 格式，而是通过 Pandoc 作为中介进行转换。因此，确保 Pandoc 已安装且路径配置正确是关键的一步。

#### 安装版 Pandoc vs 免安装版
推荐使用安装版的 Pandoc，而不是免安装的压缩包版本。这是因为安装版的 Pandoc 会自动将执行文件路径添加到系统的环境变量中，确保可以在任何终端或应用中调用 Pandoc。如果使用免安装版本，则需要手动配置环境变量，这可能导致 Mogan Research 无法正确找到 Pandoc，进而导致导出失败。

关于 `Pandoc` 插件的安装步骤，请参考 [Pandoc 二进制插件](./plugin_binary_pandoc.md)。确保 Pandoc 已正确安装后再进行 DOCX 导出操作。

### 关于导出的 DOCX 文件在 WPS 中的字体问题
Pandoc 默认使用的导出字体是针对 Microsoft Office Word 和 Apple Pages 进行优化的，因此在这些软件中查看和编辑时不会出现任何问题。然而，在 WPS 中，可能会因为缺少某些字体而导致显示问题，尤其是某些中文字符和数学公式。

#### 为什么会有字体问题？
WPS 并不像 Microsoft Word 那样预加载了所有常用字体。因此，当你在 WPS 中打开由 Pandoc 导出的 DOCX 文件时，WPS 可能无法正确渲染某些字体，导致文字显示不完整或公式乱码。

#### 解决方案
要解决此问题，用户可以手动更改 DOCX 文件的字体为 WPS 支持的字体。以下是具体步骤：
1. 打开 DOCX 文件后，选择全部文本。
2. 在字体设置中，选择适用于 WPS 的字体（例如 SimSun、Arial 或其他常见字体）。
3. 保存文件。

通过这些步骤，你可以让 WPS 正常显示导出的 DOCX 文件内容，确保文档在不同软件中保持一致的视觉效果。

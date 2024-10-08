# DOCX Data Plugin
In Mogan STEM Suite, you can export documents to DOCX format using the following steps: `File -> Export -> DOCX`.

## Prerequisite: `Pandoc`
The process of converting TeXmacs documents to DOCX format relies on the third-party tool `Pandoc`. If `Pandoc` is not properly installed and configured in your environment, an error will occur, and the export will fail.

### Why is Pandoc necessary?
Pandoc is a universal document conversion tool that enables the conversion of TeXmacs documents to DOCX format. TeXmacs itself does not directly support DOCX, but it uses Pandoc as an intermediary for conversion. Therefore, ensuring that Pandoc is installed and that the path is correctly configured is a crucial step.

### Installed Version vs. Portable Version of Pandoc
It is recommended to use the installed version of Pandoc rather than the portable version (zip package). This is because the installed version of Pandoc automatically adds the executable path to the system's environment variables, allowing it to be called from any terminal or application. If you use the portable version, you will need to manually configure the environment variables, which may lead to TeXmacs not being able to find Pandoc, causing the export to fail.

For instructions on how to install the `Pandoc` plugin, please refer to [Pandoc Binary Plugin](./plugin_binary_pandoc.md). Ensure that Pandoc is properly installed before attempting to export to DOCX.

## Font Issues in DOCX Files Exported to WPS
By default, Pandoc uses fonts that are optimized for Microsoft Office Word and Apple Pages, so viewing and editing the exported DOCX files in those applications will not cause any issues. However, when opening the DOCX file in WPS, some characters, especially Chinese characters and mathematical formulas, may not display correctly due to missing fonts.

### Why are there font issues?
WPS, unlike Microsoft Word, does not preload all commonly used fonts. Therefore, when you open a DOCX file exported by Pandoc in WPS, certain fonts may not render correctly, leading to incomplete text display or garbled mathematical formulas.

### Solution
To resolve this issue, users can manually change the fonts in the DOCX file to those supported by WPS. Here are the steps:
1. Open the DOCX file and select all the text.
2. In the font settings, choose a font that is supported by WPS (e.g., SimSun, Arial, or other common fonts).
3. Save the file.

By following these steps, you can ensure that the DOCX file displays correctly in WPS, maintaining consistent visual quality across different software platforms.

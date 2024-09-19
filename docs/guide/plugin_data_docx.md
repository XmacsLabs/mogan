# docx Data Plugin
## TeXmacs -> docx
Export method: The option to export to docx is in `File->Export->docx`.

### Prerequisite: `pandoc`
The process of converting a TeXmacs document to a docx document relies on the third-party application `pandoc`. If `pandoc` is not available in the environment during export, an error will occur, and the export will fail.

It is recommended to use the installed version of pandoc instead of the free version to circumvent problems caused by different PATH.

For information on installing the `pandoc` plugin, please refer to [Pandoc Binary Plugin](./plugin_binary_pandoc.md).

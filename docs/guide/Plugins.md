# Plugins
The plugins of Mogan can be divided into two types: built-in plugins and community plugins. According to their functions, they can be divided into:
+ **Image plugins**: These plugins provide format conversions for different image formats, such as the PDF Image plugin.
+ **Language plugins**: These plugins provide dictionaries and styles for different natural languages, such as the Chinese Language plugin.
+ **Data format plugins**: These plugins are used to process data format conversions, such as the HTML Data plugin and LaTeX Data plugin.
+ **Code plugins**: These plugins provide code highlighting definitions and corresponding styles and editor customizations for programming languages, such as the Python Code plugin.
+ **Session plugins**: These plugins provide integration with external systems and have context, such as the Maxima Session plugin.

For example, there are two types of PDF plugins: PDF data plugins and PDF image plugins. The PDF data plugin is responsible for converting PDFs with TeXmacs document attachments (extracting attachments) into TeXmacs documents or converting TeXmacs documents (exporting) into PDF documents. The PDF image plugin is responsible for converting PDF vector graphics into PNG format for correct rendering in Mogan.
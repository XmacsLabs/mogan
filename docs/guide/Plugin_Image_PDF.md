# PDF Image Plugin
The renderer of Mogan doesn't support PDF images. Such PDF images are usually PDF vector graphics, made by third-party software (such as exporting to PDF vector graphics in Octave or Maxima).

When Mogan renders PDF images, it relies on Ghostscript to convert PDF images to PNG images. Therefore, this plugin requires users to manually install Ghostscript.

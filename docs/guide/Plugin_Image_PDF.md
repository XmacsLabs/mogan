# PDF Image Plugin
The renderer of Mogan doesn't support PDF images. Such PDF images are usually PDF vector graphics, made by third-party software (such as exporting to PDF vector graphics in Octave or Maxima).

When Mogan renders PDF images, it relies on Ghostscript to convert PDF images to PNG images. Therefore, this plugin requires users to manually install Ghostscript.

## How to install Ghostscript
### Windows
Go to the [Ghostscript official website](https://www.ghostscript.com) to download the installation package and install it to the system C drive according to the default path.

If users didn't install according to the default path, they need to manually add the directory where `gs.exe` is located to the PATH.

### macOS
Use `brew install ghostscript` to install.

### Linux
Use `sudo apt install ghostscript` or other ways to install.

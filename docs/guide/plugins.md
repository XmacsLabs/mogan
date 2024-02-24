# Plugins
The plugins of Mogan can be divided into two types: built-in plugins and community plugins. According to their functions, they can be divided into:
+ **Binary plugins**: provide scheme routines to find the binary and use the binary.
+ **Image plugins**: provide format conversions for different image formats, such as the PDF Image plugin.
+ **Language plugins**: provide dictionaries and styles for different natural languages, such as the Chinese Language plugin.
+ **Data format plugins**: These plugins are used to process data format conversions, such as the HTML Data plugin and LaTeX Data plugin.
+ **Code plugins**: provide code highlighting definitions and corresponding styles and editor customizations for programming languages, such as the Python Code plugin.
+ **Session plugins**: provide integration with external systems and have context, such as the Maxima Session plugin.

## Binary Plugins
The source code of binary plugins is located in `$TEXMACS_PATH/plugins/binary`. Each executable corresponds to a source code file, with the format:

```
$TEXMACS_PATH/plugins/binary/progs/binary/<name>.scm
```

For example, the source code for the Ghostscript command `gs` is located at:

```
$TEXMACS_PATH/plugins/binary/progs/binary/gs.scm
```

If a user needs to modify the Scheme function that searches for or calls the `gs` command line, they can simply copy the above `gs.scm` file to:

```
$TEXMACS_HOME_PATH/plugins/binary/progs/binary/gs.scm
```

And then make the necessary modifications. If `gs.scm` exists under `$TEXMACS_HOME_PATH`, it will be loaded before the `gs.scm` under `$TEXMACS_PATH`.
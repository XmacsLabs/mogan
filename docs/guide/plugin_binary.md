## Binary Plugin
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

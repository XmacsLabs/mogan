# Binary Plugin
A binary plugin is a type of plugin that is primarily used for:
+ Locating the position of executable files and determining whether they exist.
+ Invoking executable files based on actual needs and encapsulating pre-processing logic for input and post-processing logic for output.

## Sharing and Exclusivity
Shared binary plugins are primarily located in `$TEXMACS_PATH/plugins/binary`, with each executable file corresponding to a source code file:
```
$TEXMACS_PATH/plugins/binary/progs/binary/<name>.scm
```
For example, the source code for Ghostscript's gs command corresponds to:
```
$TEXMACS_PATH/plugins/binary/progs/binary/gs.scm
```

Exclusive binary plugins are located in:
```
$TEXMACS_PATH/plugins/<name>/progs/binary/<name>.scm
```
For instance, the source code corresponding to the executable file for the Octave plugin is located in:
```
$TEXMACS_PATH/plugins/octave/progs/binary/octave.scm
```

## Three Essential Elements
| Function  | Purpose  |
|---|---|
| `(find-binary-xyz)` | Locates the position of the xyz executable file. If it does not exist, it returns `(url-none)`. |
| `(has-binary-xyz?)` | Determines whether the xyz executable file exists. If it exists, it returns `#t`; if not, it returns `#f`. |
| `(version-binary-xyz?)` | Returns the version of the xyz executable file. If the executable file does not exist, it returns an empty string; otherwise, it returns a string containing version information. |

## Detailed Explanation of `(find-binary-xyz)`
+ If the configuration item `plugin:binary` is set to `off`, then `(find-binary-xyz)` will return `(url-none)`.
  + If the configuration item `plugin:binary:xyz` is set to `off`, then `(find-binary-xyz)` will return `(url-none)`.
    + If the configuration item `plugin:binary:xyz` is set to `candidates-only`, it will only check the candidate paths and return the result.
      + If the configuration item `plugin:binary:xyz` is set to a path and that path exists, it will return the specified path.
      + Check the candidate paths. If any of them exist, return the candidate path.
      + Search for the executable file with the name specified in the candidate paths within the system path. If it exists, return the path; otherwise, return `(url_none)`.

The author of the binary plugin only needs to focus on whether the implementation of `(xyz-binary-candidates)` is reasonable. The configuration item `plugin:binary` currently (1.2.5.2) only supports configuration using Scheme code such as `(set-preference "plugin:binary" "/path/to/binary/xyz")`.

## Help -> Plugins -> Binaries
By clicking "Help -> Plugins -> Binaries" in Texmacs, you can find three runnable Scheme code snippets:
1. How to locate the path of a specific binary plugin.
2. How to customize the path of a specific binary plugin.
3. How to disable all binary plugins.

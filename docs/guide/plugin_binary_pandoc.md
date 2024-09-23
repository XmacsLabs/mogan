# Pandoc Binary Plugin
+ Executable filename: `pandoc`
+ Plugin source code:
  - [Codeberg](https://codeberg.org/XmacsLabs/mogan/src/branch/branch-1.2/TeXmacs/plugins/binary/progs/binary/pandoc.scm)
  - [Gitee](https://gitee.com/XmacsLabs/mogan/blob/branch-1.2/TeXmacs/plugins/binary/progs/binary/pandoc.scm)
  - [Github](https://github.com/XmacsLabs/mogan/blob/branch-1.2/TeXmacs/plugins/binary/progs/binary/pandoc.scm)
- What: Supports the `Document Conversion -> Export to Multiple Formats` feature
+ Mogan: `>= v1.2.8`

## Pandoc Introduction
+ **File Conversion**: Pandoc is a powerful tool that can convert between a wide range of markup formats, including Markdown, LaTeX, HTML, and more.
+ **Markdown Extensions**: Pandoc supports a variety of useful Markdown extensions, such as document metadata, footnotes, tables, definition lists, superscripts and subscripts, strikethroughs, enhanced ordered lists, and more.
+ **Mathematical Formulas**: Pandoc supports LaTeX math formulas, which can be rendered in HTML through various methods such as MathJax or MathML.
+ **Automatic Citations and Bibliography**: Pandoc includes a powerful system that can automatically generate citations and bibliographies, supporting multiple citation styles and bibliography formats.

## How to Install Pandoc
### Windows
Install using `Chocolatey`:

```
choco install pandoc
```
Install using `winget`:
```
winget install --source winget --exact --id JohnMacFarlane.Pandoc
```

Use the installer:  
Download and run the Pandoc installer, which will automatically add Pandoc to the system path.

Installation location:  
Typically at `C:\Program Files\Pandoc\pandoc.exe`.

### macOS
Install using `brew install pandoc`.

Installation location:  
Typically at `/usr/local/bin/pandoc`.

### Linux
On Debian bookworm, use the following command to install:


### macOS
Install using `brew install pandoc`.

### Linux
On Debian bookworm, use the following command to install:
```
sudo apt install pandoc
```

Alternatively, install using Conda:
```
conda install -c conda-forge pandoc
```

Installation location:  
Typically at `/usr/bin/pandoc` or `/usr/local/bin/pandoc`.

### How to Verify if the Installation was Successful
To verify if Pandoc was successfully installed, follow these steps:

+ Command Line Check:
  - Open a command-line terminal (CMD or PowerShell for Windows, Terminal for macOS, or Shell for Linux).
  - Type `pandoc -v` or `pandoc --version` to check the Pandoc version information. If the version number is displayed, the installation was successful.
+ Conversion Test:
  - Create a simple Markdown file (e.g., `test.md`) with the content `# Hello, Pandoc!`.
  - Run the following command in the terminal: `pandoc test.md -o test.pdf` to convert the Markdown file into a PDF. If the `test.pdf` file is generated, then Pandoc is working properly.
+ Path Check:
  - Confirm that the Pandoc installation path is added to the system's environment variables. On the command line, type `echo $PATH` (Linux and macOS) or `echo %PATH%` (Windows) to check if the Pandoc installation path is included.

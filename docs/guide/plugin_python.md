# Python Plugin
Video Tutorial in Chinese：[零基础墨干06：如何在墨干中学Python语言](https://www.bilibili.com/video/BV1hm421771h/)

## Installation
The Python plugin depends on the [Python 3 Binary Plugin](plugin_binary_python3.md). Install Python 3 according to the documentation of the Python 3 Binary Plugin, and then you can insert Python sessions.

The `ps_out` function in the Python plugin can draw Postscript vector graphics. Rendering and PDF exporting of Postscript vector graphics rely on the [Ghostscript Binary Plugin](plugin_binary_gs.md). If you have a need for vector graphics drawing, please install Ghostscript according to the documentation first.

The Python plugin supports starting from Python virtual environments created by Conda. Please install Conda and create relevant virtual environments according to the [Conda Binary Plugin](plugin_binary_conda.md) first.

## How to Start a Python Session from a Conda Virtual Environment
Assuming you have already installed Miniconda and the Conda Binary Plugin can correctly detect the installed Conda.

You can create a Python virtual environment named `plot` with the following command:
```
conda create -n plot python matplotlib
```
In this `plot` virtual environment, we have preinstalled the matplotlib Python package.

Then, you can start a Python session based on the `plot` Python virtual environment through `Plugins -> Sessions -> Python -> conda_plot`. In this Python session, you can `import matplotlib` and then call functions from the matplotlib package to draw graphs. If the `conda_plot` option does not appear in `Plugins -> Sessions -> Python`, you can restart Mogan or click `Tools -> Update -> Plugins` and perform the relevant operations again.

## Check built-in Doc at `Help -> Plugins -> Python`

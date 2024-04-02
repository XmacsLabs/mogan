#!/usr/bin/env python
###############################################################################
#
# MODULE      : tm_python.py
# DESCRIPTION : Initialize python plugin
# COPYRIGHT   : (C) 2004       Ero Carrera, ero@dkbza.org
#               (C) 2012       Adrian Soto
#               (C) 2014       Miguel de Benito Delgado, mdbenito@texmacs.org
#               (C) 2018-2020  Darcy Shen
#
# This software falls under the GNU general public license version 3 or later.
# It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
# in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.

import ast
import os
import platform
import sys
from inspect import getsource, getsourcefile

from tmpy.capture import CaptureStdout
from tmpy.completion import complete, parse_complete_command
from tmpy.postscript import FileOutDummy, PSOutDummy, pdf_out, ps_out
from tmpy.protocol import (
    DATA_COMMAND,
    flush_command,
    flush_file,
    flush_prompt,
    flush_ps,
    flush_scheme,
    flush_verbatim,
)

# import logging as log
# log.basicConfig(filename='/tmp/tm_python.log',level=log.INFO)


def flush_output(data):
    """Do some parsing on the output according to its type.

    Non printable characters in unicode strings are escaped and objects
    of type None are not printed (so that procedure calls, as opposed to
    function calls, don't produce any output).
    """

    if data is None:
        flush_verbatim("")
        return

    if isinstance(data, PSOutDummy):
        flush_ps(data.content)
    elif isinstance(data, FileOutDummy):
        if data.content is None:
            flush_verbatim("")
        else:
            flush_file(data.content)
    else:
        flush_verbatim(str(data).strip())


def as_scm_string(text):
    return '"%s"' % text.replace("\\", "\\\\").replace('"', '\\"')


def compile_help(text):
    cmd = "help(%s)" % text
    out = {"help": "", "src": "", "file": ""}

    try:
        out["help"] = CaptureStdout.capture(cmd, my_globals, "tm_python")
    except Exception as e:
        out["help"] = 'No help for "%s": %s' % (text, e)

    try:
        out["src"] = eval(
            "getsource(%s)" % text, my_globals, {"getsource": getsource}
        )
    except Exception as e:
        out["src"] = 'No code available for "%s": %s' % (text, e)

    try:
        # Todo: strip docstring from code
        out["file"] = eval(
            "getsourcefile(%s)" % text,
            my_globals,
            {"getsourcefile": getsourcefile},
        )
    except Exception as e:
        out["file"] = 'Unable to access the code for "%s": %s' % (text, e)

    return dict(map(lambda k_v: (k_v[0], as_scm_string(k_v[1])), out.items()))


def my_eval(code, p_globals):
    """Execute a script and return the value of the last expression."""

    block = ast.parse(code, mode="exec")
    if len(block.body) > 1 and isinstance(block.body[-1], ast.Expr):
        last = ast.Expression(block.body.pop().value)
        exec(compile(block, "<string>", mode="exec"), p_globals)
        return eval(compile(last, "<string>", mode="eval"), p_globals)
    else:
        return eval(code, p_globals)


__version__ = "3.0"
__author__ = "Ero Carrera, Adrian Soto, Miguel de Benito Delgado, Darcy Shen"
my_globals = {}

# We insert into the session's namespace the 'ps_out' method.
my_globals["ps_out"] = ps_out
my_globals["pdf_out"] = pdf_out

# As well as some documentation.
my_globals[
    "__doc__"
] = """A Python plugin for TeXmacs.
Provides autocompletion and embedding of PostScript data into the document,
e.g from files or from matplotlib.pyplot.
A rudimentary help window is also implemented: type the name of an object
with a question mark at the end to use it."""

text = "import builtins as __builtins__"
CaptureStdout.capture(text, my_globals, "tm_python")

sys.stdout = os.fdopen(sys.stdout.fileno(), "w")

###############################################################################
# Session start
###############################################################################
flush_verbatim(
    f"""Python { platform.python_version() } [{ sys.executable }]
Python plugin for TeXmacs.
Please see the documentation in Help -> Plugins -> Python
"""
)
flush_prompt(">>> ")

while True:
    line = input()
    if not line:
        continue
    if line[0] == DATA_COMMAND:
        sf = parse_complete_command(line[1:])
        if sf[0] == "complete":
            flush_scheme(complete(sf[1], sf[2], my_globals))
        continue
    elif line.endswith("?") and not line.strip().startswith("#"):
        if len(line) > 1:
            out = compile_help(line[:-1])
            flush_command(
                "(tmpy-open-help %s %s %s)"
                % (out["help"], out["src"], out["file"])
            )
        else:
            flush_verbatim('Type a name before the "?" to see the help')
        continue
    else:
        lines = [line]
        while line != "<EOF>":
            line = input()
            if line == "":
                continue
            lines.append(line)
        text = "\n".join(lines[:-1])
        try:  # Is it an expression?
            result = my_eval(text, my_globals)
        except Exception:
            result = CaptureStdout.capture(text, my_globals, "python")
        flush_output(result)

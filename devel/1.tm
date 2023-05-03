<TeXmacs|2.1.2>

<style|tmdoc>

<\body>
  <tmdoc-title|Cleaning for Mogan Editor 1.2.x>

  Mogan Editor 1.2.x is forked from GNU <TeXmacs> 2.1.2. The first project of
  Mogan Editor is removing the un-used code in GNU <TeXmacs>:

  <\itemize>
    <item>Build Tools: we will focus on the xmake build tools

    <item>Plugins

    <\itemize>
      <item>Plugins for non-free software will be removed

      <item>Unmaintained plugins will be removed

      <item>Python-implemented plugins will be removed
    </itemize>

    <item>UI frameworks: focus on the Qt framework

    <item>Packager Scripts: focus on Debian, macOS and Windows

    <item>Fonts: should not be shipped with the software

    <item>Examples: example texts and plugins should be removed
  </itemize>

  <section|Tasks>

  Tasks in this project must be completed via pull requests which only
  contains file deletion commits. (If it does not only contains file
  deletions, please separate it in several pull requests.) For Mogan Editor
  v1.3.x, we will fork the latest codebase of GNU <TeXmacs> again. And the
  first project of Mogan Editor v1.3.x will be cleaning for Mogan Editor
  1.3.x.

  <\wide-tabular>
    <tformat|<cwith|1|1|1|-1|cell-tborder|0ln>|<cwith|1|1|1|-1|cell-bborder|0ln>|<cwith|2|2|1|-1|cell-tborder|0ln>|<cwith|1|1|1|1|cell-lborder|0ln>|<cwith|1|1|2|2|cell-rborder|0ln>|<table|<row|<\cell>
      1_1
    </cell>|<\cell>
      Remove the support of Autotools
    </cell>>|<row|<\cell>
      1_2
    </cell>|<\cell>
      Remove the support of CMake
    </cell>>|<row|<\cell>
      1_3
    </cell>|<\cell>
      Remove the Cocoa integration
    </cell>>|<row|<\cell>
      1_4
    </cell>|<\cell>
      Remove the Widkit integration
    </cell>>|<row|<\cell>
      1_5
    </cell>|<\cell>
      Remove the X11 integration
    </cell>>|<row|<\cell>
      1_6
    </cell>|<\cell>
      Remove the Matlab plugin
    </cell>>|<row|<\cell>
      1_7
    </cell>|<\cell>
      Remove the Mathematica plugin
    </cell>>|<row|<\cell>
      1_8
    </cell>|<\cell>
      Remove the Coq plugin
    </cell>>|<row|<\cell>
      1_9
    </cell>|<\cell>
      Remove the Maple plugin
    </cell>>|<row|<\cell>
      1_10
    </cell>|<\cell>
      Remove the plugins implemented in Python
    </cell>>|<row|<\cell>
      1_11
    </cell>|<\cell>
      Remove packager scripts for un-supported OS\ 
    </cell>>|<row|<\cell>
      1_12
    </cell>|<\cell>
      Remove the lush plugin
    </cell>>|<row|<\cell>
      1_13
    </cell>|<\cell>
      Remove the equation_editor plugin
    </cell>>|<row|<\cell>
      1_14
    </cell>|<\cell>
      Remove the R plugin
    </cell>>|<row|<\cell>
      1_15
    </cell>|<\cell>
      Remove the Axiom plugin
    </cell>>|<row|<\cell>
      1_16
    </cell>|<\cell>
      Remove the Libertine font
    </cell>>|<row|<\cell>
      1_17
    </cell>|<\cell>
      Remove the python-implemented example plugins
    </cell>>|<row|<\cell>
      1_18
    </cell>|<\cell>
      Remove the example texts
    </cell>>|<row|<\cell>
      1_19
    </cell>|<\cell>
      Remove the feynmf plugin
    </cell>>|<row|<\cell>
      1_20
    </cell>|<\cell>
      Remove the Scilab plugin
    </cell>>|<row|<\cell>
      1_21
    </cell>|<\cell>
      Remove the texgraph plugin
    </cell>>|<row|<\cell>
      1_22
    </cell>|<\cell>
      Remove the lisp plugin
    </cell>>|<row|<\cell>
      1_23
    </cell>|<\cell>
      Remove the reduce plugin
    </cell>>|<row|<\cell>
      1_24
    </cell>|<\cell>
      Remove the mupad plugin
    </cell>>|<row|<\cell>
      1_25
    </cell>|<\cell>
      Remove the cococa5 plugin
    </cell>>|<row|<\cell>
      1_26
    </cell>|<\cell>
      Remove the yacas plugin
    </cell>>|<row|<\cell>
      1_27
    </cell>|<\cell>
      Remove mathemagix related plugins
    </cell>>|<row|<\cell>
      1_28
    </cell>|<\cell>
      Remove the mycas plugin
    </cell>>|<row|<\cell>
      1_29
    </cell>|<\cell>
      Remove the qcl plugin
    </cell>>|<row|<\cell>
      1_30
    </cell>|<\cell>
      Remove the macaulay2 plugin
    </cell>>|<row|<\cell>
      1_31
    </cell>|<\cell>
      Remove the gtybalt plugin
    </cell>>|<row|<\cell>
      1_32
    </cell>|<\cell>
      Remove the Axel C++ interface
    </cell>>|<row|<\cell>
      1_33
    </cell>|<\cell>
      Remove the Cairo C++ interface
    </cell>>|<row|<\cell>
      1_34
    </cell>|<\cell>
      Remove the Imlib2 C++ interface
    </cell>>|<row|<\cell>
      1_35
    </cell>|<\cell>
      Remove the Fira fonts
    </cell>>|<row|<\cell>
      1_36
    </cell>|<\cell>
      Remove the Mplayer C++ interface
    </cell>>|<row|<\cell>
      1_37
    </cell>|<\cell>
      Remove docs in plain text for GNU <TeXmacs>
    </cell>>|<row|<\cell>
      1_38
    </cell>|<\cell>
      Remove the texgyre fonts
    </cell>>|<row|<\cell>
      1_39
    </cell>|<\cell>
      Remove the Stix fonts
    </cell>>>>
  </wide-tabular>

  <tmdoc-copyright|2023|Darcy Shen>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<\initial>
  <\collection>
    <associate|page-medium|papyrus>
  </collection>
</initial>
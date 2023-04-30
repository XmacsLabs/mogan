<TeXmacs|2.1.2>

<style|tmdoc>

<\body>
  <tmdoc-title|Cleaning for Mogan Editor 1.2.0>

  Mogan Editor 1.2.0 is forked from GNU <TeXmacs> 2.1.2. The first project of
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
  </itemize>

  <section|Tasks>

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
      <todo|1_12>
    </cell>|<\cell>
      Remove the lush plugin
    </cell>>|<row|<\cell>
      <todo|1_13>
    </cell>|<\cell>
      Remove the equation_editor plugin
    </cell>>|<row|<\cell>
      <todo|1_14>
    </cell>|<\cell>
      Remove the R plugin
    </cell>>|<row|<\cell>
      <todo|1_15>
    </cell>|<\cell>
      Remove the Axiom plugin
    </cell>>>>
  </wide-tabular>

  \;

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
    <associate|page-medium|paper>
  </collection>
</initial>
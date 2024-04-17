<TeXmacs|2.1.2>

<style|<tuple|tmdoc|dot|british>>

<\body>
  <tmdoc-title|DOT Code Plugin>

  <paragraph|DOT style package>

  Click <menu|Document|Style|Add package|Code|dot> or click
  <menu|<icon|tm_add.svg>|Code|physics> on the focus toolbar to add the DOT
  style package.

  In the DOT style package, <markup|dot-lang> is provided for inline DOT
  snippets and <markup|dot-code> is provided for DOT code blocks.
  <markup|dot> is already defined, that's why we are using <markup|dot-lang>
  for inline code.

  <paragraph|DOT syntax>

  The syntax of the DOT language is defined in:

  <slink|$TEXMACS_PATH/plugins/dot/progs/code/dot-lang.scm>

  Here is an example dot code block with syntax highlight:

  <\dot-code>
    digraph G {

    \ \ \ \ Hello-\<gtr\>World

    }
  </dot-code>

  <paragraph|DOT editing>

  The editing of the DOC code is defined in:

  <slink|$TEXMACS_PATH/plugins/dot/progs/code/dot-edit.scm>

  <tmdoc-copyright|2024|Darcy Shen>

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
    <associate|page-screen-margin|true>
  </collection>
</initial>
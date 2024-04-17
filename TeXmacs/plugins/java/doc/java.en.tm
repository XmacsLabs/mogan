<TeXmacs|2.1.2>

<style|<tuple|tmdoc|british|java>>

<\body>
  <\hide-preamble>
    <assign|code-name|java>

    <assign|code-Name|Java>
  </hide-preamble>

  <tmdoc-title|<value|code-Name> Code Plugin>

  <paragraph|<value|code-Name> style package>

  Click <menu|Document|Style|Add package|Code|<value|code-name>> or click
  <menu|<icon|tm_add.svg>|Code|<value|code-name>> on the focus toolbar to add
  the <value|code-Name> style package.

  In the <code-Name> style package, <markup|<value|code-name>> is provided
  for inline snippets and <markup|<value|code-name>-code> is provided for
  <code-Name> code blocks.

  <paragraph|<value|code-Name> syntax>

  The syntax of the <value|code-Name> language is defined in:

  <slink|$TEXMACS_PATH/plugins/dot/progs/code/<value|code-name>-lang.scm>

  Here is an example <value|code-Name> code block with syntax highlight:

  <\java-code>
    public class HelloWorld {

    \ \ \ \ public static void main(String[] args) {

    \ \ \ \ \ \ \ \ System.out.println("Hello, World!");

    \ \ \ \ }

    }
  </java-code>

  <paragraph|<value|code-Name> editing>

  The editing of the <value|code-Name> code is defined in:

  <slink|$TEXMACS_PATH/plugins/<value|code-name>/progs/code/<value|code-name>-edit.scm>

  Here is a list of key bindings:

  <\description>
    <item*|<key|p s v m var>>

    <\java-code>
      public static int main() {

      \ \ \ \ 

      }
    </java-code>
  </description>

  \;

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
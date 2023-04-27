<TeXmacs|2.1.2>

<style|tmdoc>

<\body>
  <tmdoc-title|Use <shell|XDG_DATA_HOME> on GNU/Linux>

  <section|Feature metadata>

  <\itemize>
    <item>Owner: Darcy Shen
  </itemize>

  <section|Description>

  <subsection|What>

  According to the XDG Base Directory Specification v0.8<\footnote>
    <slink|https://specifications.freedesktop.org/basedir-spec/0.8/ar01s02.html>
  </footnote>:

  <\quote-env>
    There is a single base directory relative to which user-specific data
    files should be written. This directory is defined by the environment
    variable $XDG_DATA_HOME.
  </quote-env>

  <subsection|Why>

  To submit Mogan Editor to the UOS App Store.

  <subsection|How>

  <\description>
    <item*|plugins><shell|$HOME/.Xmacs/plugins> \<rightarrow\>
    <shell|$XDG_DATA_PATH/app.mogan.editor/plugins>

    <\itemize>
      <item>If <shell|$XDG_DATA_PATH/app.mogan.editor/plugins> is emtpy, it
      will be created.
    </itemize>
  </description>

  <section|How to test>

  <\big-table|<tabular|<tformat|<table|<row|<cell|Tester>|<cell|Platform>|<cell|Version>|<cell|How>>|<row|<cell|>|<cell|GNU/Linux>|<cell|>|<cell|>>|<row|<cell|>|<cell|macOS>|<cell|>|<cell|>>|<row|<cell|>|<cell|Windows>|<cell|>|<cell|>>>>>>
    Check table for testing
  </big-table>
</body>

<\initial>
  <\collection>
    <associate|page-medium|paper>
  </collection>
</initial>
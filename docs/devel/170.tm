<TeXmacs|2.1.2>

<style|tmdoc>

<\body>
  <tmdoc-title|Use <shell|XDG_CACHE_HOME> on GNU/Linux>

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
    There is a single base directory relative to which user-specific
    non-essential (cached) data should be written. This directory is defined
    by the environment variable <code*|$XDG_CACHE_HOME>.
  </quote-env>

  <subsection|Why>

  To submit Mogan Editor to the UOS App Store.

  <subsection|How>

  The caches under <shell|$HOME/.Xmacs/system/cache> should be placed at
  <shell|$XDG_CACHE_HOME>. If the environment variable is not available, it
  should default to <shell|$HOME/.cache>.

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
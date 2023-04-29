<TeXmacs|2.1.2>

<style|<tuple|tmdoc|british>>

<\body>
  <tmdoc-title|Import xmake build definition before the kernel decouple from
  branch 1.1>

  <section|Feature metadata>

  <\itemize>
    <item>Owner: Darcy Shen
  </itemize>

  <section|Description>

  <subsection|What>

  xmake support for Mogan Editor 1.1.2 is completed mainly by jinkaimori and
  Darcy. While we start to add the xmake support for branch 1.2.x, we need to
  add the basic xmake support first, and then improve it step by step.

  <subsection|Why>

  For the first pull request, Darcy chose to import the xmake build before
  the kernel decouple to make the pull request as small as enough.

  <subsection|How>

  For the first pull request, we only make it work for GNU/Linux:

  <\shell-code>
    xmake build mogan
  </shell-code>
</body>

<initial|<\collection>
</collection>>
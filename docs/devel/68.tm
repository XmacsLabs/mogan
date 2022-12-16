<TeXmacs|2.1.2>

<style|<tuple|tmdoc|british>>

<\body>
  <tmdoc-title|WebP Support>

  <section|Feature metadata>

  <\itemize>
    <item>Reporter: jingkaimori on <hlink|Gitee|https://gitee.com/XmacsLabs/mogan/issues/I5E7YD>

    <item>Owner: Darcy Shen

    <item>Tester: Darcy Shen
  </itemize>

  <section|Description>

  Using the WEBP format, the size of the image could be smaller than PNG.

  <\big-figure|<tabular*|<tformat|<twith|table-width|1par>|<twith|table-hmode|exact>|<cwith|2|2|1|1|cell-valign|c>|<cwith|4|4|1|1|cell-valign|c>|<table|<row|<cell|Actual
  Size>|<cell|WEBP>|<cell|PNG>>|<row|<cell|64x64>|<cell|<image|../../TeXmacs/misc/images/texmacs-64.webp|64pt|64pt||>>|<cell|<image|../../TeXmacs/misc/images/texmacs-64.png|64pt|64pt||>>>|<row|<cell|>|<cell|1.6K>|<cell|5.9k>>|<row|<cell|128x128>|<cell|<image|../../TeXmacs/misc/images/texmacs-128.webp|64pt|64pt||>>|<cell|<image|../../TeXmacs/misc/images/texmacs-128.png|64pt|64pt||>>>|<row|<cell|>|<cell|3.5K>|<cell|13K>>>>>>
    A Comparison between <name|WebP> and <name|PNG>
  </big-figure>

  The <verbatim|webp> image format is supported via Qt. We manually disable
  it in macOS related code.

  <section|How to test>

  <subsection|Scheme Unit Test>

  <\session|scheme|default>
    <\input|Scheme] >
      (regtest-tm-convert)
    </input>

    <\input|Scheme] >
      \;
    </input>
  </session>

  <subsection|C++ Unit Test>

  <\shell-code>
    xmake build qt_utilities_test && xmake run qt_utilities_test
  </shell-code>

  <subsection|PDF Export>

  Export this file to PDF and then check the exported PDF file.

  <subsection|PS Export>

  Export this file to PS and then check the exported PS file.

  \;

  \;
</body>

<initial|<\collection>
</collection>>
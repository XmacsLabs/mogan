<TeXmacs|2.1.2>

<style|<tuple|tmdoc|british>>

<\body>
  <tmdoc-title|Set Noto Serif CJK as Default Chinese Font on GNU/Linux>

  Previously, FandolSong is the default Chinese font on GNU/Linux for Mogan
  Editor.

  With this Pull Request merged, the default Chinese font will be Noto Serif
  CJK SC. Because Noto Serif CJK SC is the default Chinese font for
  LibreOffice Writer on Debian.

  <section|How to test>

  <subsection|Unit test>

  <\shell-code>
    xmake build font_test

    xmake run font_test
  </shell-code>

  <subsection|Scheme Session>

  <\session|scheme|default>
    <\unfolded-io|Scheme] >
      (default-chinese-font)
    <|unfolded-io>
      "Noto Serif CJK SC"
    </unfolded-io>

    <\input|Scheme] >
      \;
    </input>
  </session>

  <subsection|Manually>

  Create a Chinese document and then exported it to PDF. You will find the
  font info in the PDF metadata.

  <tmdoc-copyright|2022|Darcy Shen>

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
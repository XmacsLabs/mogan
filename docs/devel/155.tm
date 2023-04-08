<TeXmacs|2.1.2>

<style|<tuple|tmdoc|british>>

<\body>
  <tmdoc-title|Should prefer the embedded gs binary>

  <section|Bug Metadata>

  <\itemize>
    <item>Reporter: ProfFan

    <item>Owner: Darcy Shen

    <item>Tester: Darcy Shen
  </itemize>

  <section|How to reproduce it>

  <\session|shell|default>
    <\output>
      Shell session inside TeXmacs pid = 16408
    </output>

    <\unfolded-io|Shell] >
      /Applications/Mogan.app/Contents/Resources/share/Xmacs/bin/gs --version
    <|unfolded-io>
      9.54.0
    </unfolded-io>

    <\unfolded-io|Shell] >
      /opt/homebrew/bin/gs --version
    <|unfolded-io>
      9.56.1
    </unfolded-io>

    <\input|Shell] >
      \;
    </input>
  </session>

  The embeded gs version is 9.54.0 and the system gs version is 9.56.1.

  Enable <menu|Debug|convert> and click <menu|Help|Plug-ins|Python>, we can
  find the following output:

  <\shell-code>
    TeXmacs] debug-convert, "gs" -dQUIET -dNOPAUSE -dBATCH -dSAFER
    -sDEVICE=pngalpha -dGraphicsAlphaBits=4 -dTextAlphaBits=4 -g1196x896
    -sOutputFile=/Users/da/.Xmacs/system/tmp/16277/tmp_438265816.png
    -r186x186 -c " -75 -223 translate gsave " \ -f
    /Users/da/.Xmacs/system/tmp/16277/tmp_1147139347.ps -c " grestore "
  </shell-code>

  It indicates that we are using the system gs. And the embedded gs should be
  the preferred one to avoid security issues or bugs in the system one.

  <section|Description>

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
    <associate|page-medium|papyrus>
  </collection>
</initial>
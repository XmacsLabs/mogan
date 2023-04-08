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

  <section|How to test it>

  Enable <menu|Debug|convert> and click <menu|Help|Plug-ins|Python> after the
  fix.

  To test it on macOS, here is the command line to launch Mogan (because gs
  binary is only available in the Mogan.dmg, but it is not available when we
  are developing)

  <\shell-code>
    TEXMACS_PATH=/Applications/Mogan.app/Contents/Resources/share/Xmacs
    ./build/macosx/arm64/release/Mogan.app/Contents/MacOS/Mogan
  </shell-code>

  Here the result on macOS:

  <\shell-code>
    TeXmacs] debug-convert, "/Applications/Mogan.app/Contents/Resources/share/Xmacs/bin/gs"
    -dQUIET -dNOPAUSE -dBATCH -dSAFER -sDEVICE=pngalpha -dGraphicsAlphaBits=4
    -dTextAlphaBits=4 -g1196x896 -sOutputFile=/Users/da/.Xmacs/system/tmp/32841/tmp_1611070864.png
    -r186x186 -c " -75 -223 translate gsave " \ -f
    /Users/da/.Xmacs/system/tmp/32841/tmp_1269862822.ps -c " grestore "
  </shell-code>

  Here is the result on GNU/Linux:

  <\shell-code>
    TeXmacs] debug-convert, "gs" -dQUIET -dNOPAUSE -dBATCH -dSAFER
    -sDEVICE=pngalpha -dGraphicsAlphaBits=4 -dTextAlphaBits=4 -g674x505
    -sOutputFile=/home/sadhen/.Xmacs/system/tmp/1816835/tmp_1797103931.png
    -r105x105 -c " -75 -223 translate gsave " \ -f
    /home/sadhen/.Xmacs/system/tmp/1816835/tmp_2042949476.ps -c " grestore "
  </shell-code>

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
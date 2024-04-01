<TeXmacs|2.1.2>

<style|<tuple|tmdoc|english|old-spacing|old-dots|old-lengths>>

<\body>
  <tmdoc-title|Installation>

  It is a built-in plugin in GNU <TeXmacs> and Mogan Research. If
  <menu|Insert|Session|Octave> is not available, do the following steps to
  fix it:

  <\enumerate>
    <item>Check if Ghostscript and GNU Octave is installed, if not, please
    install Ghostscript and GNU Octave first. GNU Octave 5.x is recommended.
    Plotting in <TeXmacs> for GNU Octave (\<geqslant\>6.x) on Windows is
    broken.

    <item>Check the location and version of the gs and octave binary:

    <\session|scheme|default>
      <\unfolded-io|Scheme] >
        (find-binary-gs)
      <|unfolded-io>
        \<less\>url /opt/homebrew/Cellar/ghostscript/10.02.1/bin/gs\<gtr\>
      </unfolded-io>

      <\unfolded-io|Scheme] >
        (version-binary-gs)
      <|unfolded-io>
        10.02.1
      </unfolded-io>

      <\unfolded-io|Scheme] >
        (find-binary-octave)
      <|unfolded-io>
        \<less\>url /usr/bin/octave-cli\<gtr\>
      </unfolded-io>

      <\unfolded-io|Scheme] >
        (version-binary-octave)
      <|unfolded-io>
        GNU Octave, version 8.4.0
      </unfolded-io>

      <\input|Scheme] >
        \;
      </input>
    </session>
  </enumerate>

  <tmdoc-copyright|2020\U2024|Darcy Shen>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<initial|<\collection>
</collection>>
<TeXmacs|2.1.2>

<style|<tuple|tmdoc|english|old-spacing|old-dots|old-lengths>>

<\body>
  <tmdoc-title|Installation>

  It is a built-in plugin in GNU <TeXmacs> and Mogan Research. If
  <menu|Insert|Session|Octave> is not available, do the following steps to
  fix it:

  <\enumerate>
    <item>Check if GNU Octave is installed, if not, please install GNU Octave
    first. GNU Octave 5.x is recommended. Plotting in <TeXmacs> for GNU
    Octave 6.x on Windows is broken.

    <item>Check the location and version of the octave binary:

    <\session|scheme|default>
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

    if not, please add the directory where <shell|octave-cli> exists to the
    system path.
  </enumerate>

  If you need to customize it, please install it under the
  <code*|$TEXMACS_HOME_PATH>:

  <\shell-code>
    # For GNU/Linux or macOS

    git clone https://github.com/texmacs/octave.git
    $HOME/.TeXmacs/plugins/octave

    \;

    # For Windows

    git clone https://github.com/texmacs/octave.git
    %APPDATA%\\TeXmacs\\plugins\\octave
  </shell-code>

  For users who does not know about Git, please download the zip from Github
  and unzip the <shell|master.zip> to the corresponding directory.

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
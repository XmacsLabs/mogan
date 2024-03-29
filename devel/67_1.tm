<TeXmacs|2.1.2>

<style|<tuple|tmdoc|british>>

<\body>
  <tmdoc-title|Octave plugin is not activiated on windows>

  <section|Bug Metadata>

  <\itemize>
    <item>Owner: Darcy Shen

    <item>Tester: Darcy Shen
  </itemize>

  <section|Description>

  Here is the scheme entry of the octave plugin:

  <\slink>
    $TEXMACS_PATH/plugins/octave/progs/init-octave.scm
  </slink>

  The follow scheme snippet must be true to activate the octave plugin:

  <\session|scheme|default>
    <\unfolded-io|Scheme] >
      (or (url-exists-in-path? "octave")

      \ \ \ \ (url-exists-in-path? "octave-octave-app"))
    <|unfolded-io>
      #f
    </unfolded-io>

    <\input|Scheme] >
      \;
    </input>
  </session>

  It is false now. According to the default installation of GNU Octave 8.3.0,
  this path <shell|C:\\Program Files\\GNU Octave\\Octave-8.3.0> should be
  inserted into <shell|$PATH>.

  Checked all paths which containing <shell|"octave">, this PATH is not
  available.

  <\session|scheme|default>
    <\unfolded-io|Scheme] >
      (filter

      \ (lambda (x) (string-contains? x "Octave"))

      \ (string-split (system-getenv "PATH") #\\;))
    <|unfolded-io>
      ("C:\\\\Program Files\\\\GNU Octave\\\\Octave-8.3.0\\\\mingw64\\\\bin")
    </unfolded-io>

    <\input|Scheme] >
      \;
    </input>
  </session>

  Now, we can set the path manually:

  <\session|scheme|default>
    <\input|Scheme] >
      (plugin-add-windows-path "GNU Octave/Octave*/mingw64" "bin" #t)
    </input>

    <\unfolded-io|Scheme] >
      (supports-octave?)
    <|unfolded-io>
      #t
    </unfolded-io>

    <\unfolded-io|Scheme] >
      (url-append "GNU Octave/Octave*/mingw64" "bin")
    <|unfolded-io>
      \<less\>url GNU Octave\\{Octave*}\\mingw64\\bin\<gtr\>
    </unfolded-io>

    <\unfolded-io|Scheme] >
      (url-append (system-\<gtr\>url "C:\\\\Program File*")

      \ \ \ \ \ \ \ \ \ \ \ \ (url-append "GNU Octave/Octave*/mingw64"
      "bin"))
    <|unfolded-io>
      \<less\>url C:\\{Program File*}\\GNU
      Octave\\{Octave*}\\mingw64\\bin\<gtr\>
    </unfolded-io>

    <\unfolded-io|Scheme] >
      (url-complete

      \ (url-append (system-\<gtr\>url "C:\\\\Program File*")

      \ \ \ \ \ \ \ \ \ \ \ \ \ (url-append "GNU Octave/Octave*/mingw64"
      "bin"))

      \ "dr")
    <|unfolded-io>
      \<less\>url C:\\Program Files\\GNU Octave\\Octave-8.3.0\\mingw64\\bin\<gtr\>
    </unfolded-io>

    <\input|Scheme] >
      \;
    </input>
  </session>

  Use <menu|Tool|Update|Plugin> to refresh the plugin menu. It works now.

  <scm|plugin-add-windows-path> works fine, but <scm|:winpath> does not work,
  because it is only work when <scm|(os-mingw?)> is true.

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
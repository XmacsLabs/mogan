<TeXmacs|2.1.2>

<style|<tuple|tmdoc|british>>

<\body>
  <tmdoc-title|Maxima plugin is not activiated>

  <section|Bug Metadata>

  <\itemize>
    <item>Owner: Darcy Shen

    <item>Tester: Darcy Shen
  </itemize>

  <section|Description>

  The follow scheme snippet must be true to activate the maxima plugin:

  <\session|scheme|default>
    <\unfolded-io|Scheme] >
      (or (url-exists-in-path? "maxima.bat") (url-exists-in-path? "maxima"))
    <|unfolded-io>
      #f
    </unfolded-io>

    <\input|Scheme] >
      \;
    </input>
  </session>

  It is false now. According to the default installation of Maxima 5.47.0,
  this path <shell|C:\\maxima-5.47.0\\bin> should be inserted into
  <shell|$PATH>.

  Checked all paths which containing <shell|"maxima">, this PATH is not
  available.

  <\session|scheme|default>
    <\folded-io|Scheme] >
      (filter

      \ (lambda (x) (string-contains? x "maxima"))

      \ (string-split (system-getenv "PATH") #\\;))
    <|folded-io>
      ("C:\\\\Users\\\\darcy\\\\git\\\\mogan\\\\build\\\\packages\\\\app.mogan\\\\data\\\\plugins\\\\maxima\\\\bin")
    </folded-io>

    <\input|Scheme] >
      \;
    </input>
  </session>

  Now, we can set the path manually:

  <\session|scheme|default>
    <\input|Scheme] >
      (plugin-add-windows-path "maxima*" "bin" #t)
    </input>

    <\input|Scheme] >
      \;
    </input>
  </session>

  It does not work, because it only works for mingw.

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
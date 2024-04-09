<TeXmacs|2.1.2>

<style|<tuple|generic|no-page-numbers|british>>

<\body>
  <section|Test on macOS>

  <\session|scheme|default>
    <\input|Scheme] >
      (reset-preference "plugin:binary:gs")
    </input>

    <\unfolded-io|Scheme] >
      (find-binary-gs)
    <|unfolded-io>
      \<less\>url /usr/bin/gs\<gtr\>
    </unfolded-io>

    <\input|Scheme] >
      (set-preference "plugin:binary:gs" "/bin/gs")
    </input>

    <\unfolded-io|Scheme] >
      (find-binary-gs)
    <|unfolded-io>
      \<less\>url /bin/gs\<gtr\>
    </unfolded-io>

    <\input|Scheme] >
      (set-preference "plugin:binary:gs" "/Applications/TeXmacs.app/Contents/Resources/share/TeXmacs/bin/gs")
    </input>

    <\unfolded-io|Scheme] >
      (find-binary-gs)
    <|unfolded-io>
      \<less\>url /Applications/TeXmacs.app/Contents/Resources/share/TeXmacs/bin/gs\<gtr\>
    </unfolded-io>

    <\input|Scheme] >
      (set-preference "plugin:binary:gs" "/bin")
    </input>

    <\unfolded-io|Scheme] >
      (find-binary-gs)
    <|unfolded-io>
      \<less\>url /opt/homebrew/Cellar/ghostscript/10.02.1/bin/gs\<gtr\>
    </unfolded-io>

    <\input|Scheme] >
      \;
    </input>
  </session>

  <section|Test on Windows>

  <\session|scheme|default>
    <\unfolded-io|Scheme] >
      (use-modules (binary gs) (binary common))
    <|unfolded-io>
      (#1=(inlet '$tmapidoc $tmapidoc 'with-remote-identifier
      with-remote-identifier 'with-remote-get-user-name
      with-remote-get-user-name 'with-remote-get-user-pseudo
      with-remote-get-user-pseudo 'with-remote-search-user
      with-remote-search-user 'with-remote-search with-remote-search
      'with-remote-create-entry with-remote-create-entry
      'with-remote-get-entry with-remote-get-entry
      'with-remote-get-attributes with-remote-get-attributes
      'with-remote-get-field with-remote-get-field 'tm-call-back tm-call-back
      'with-remote-context with-remote-context ...) #1#)
    </unfolded-io>

    <\input|Scheme] >
      (reset-preference "plugin:binary:gs")
    </input>

    <\unfolded-io|Scheme] >
      (find-binary-gs)
    <|unfolded-io>
      \<less\>url C:\\Program Files\\gs\\gs10.01.2\\bin\\gswin64c.exe\<gtr\>
    </unfolded-io>

    <\input|Scheme] >
      (set-preference "plugin:binary:gs" "/bin/gs")
    </input>

    <\unfolded-io|Scheme] >
      (find-binary-gs)
    <|unfolded-io>
      \<less\>url C:\\Program Files\\gs\\gs10.01.2\\bin\\gswin64c.exe\<gtr\>
    </unfolded-io>

    <\input|Scheme] >
      (set-preference "plugin:binary:gs" "C:\\\\Program Files
      (x86)\\\\TeXmacs\\\\bin\\\\gs.exe")
    </input>

    <\unfolded-io|Scheme] >
      (find-binary-gs)
    <|unfolded-io>
      \<less\>url C:\\Program Files (x86)\\TeXmacs\\bin\\gs.exe\<gtr\>
    </unfolded-io>

    <\unfolded-io|Scheme] >
      (version-binary-gs)
    <|unfolded-io>
      9.24
    </unfolded-io>

    <\input|Scheme] >
      (reset-preference "plugin:binary:gs")
    </input>

    <\input|Scheme] >
      (system-setenv "PATH" (string-append (system-getenv "PATH") ";"
      "C:\\\\Program Files (x86)\\\\TeXmacs\\\\bin\\\\"))
    </input>

    <\unfolded-io|Scheme] >
      (find-binary (list ) "gs")
    <|unfolded-io>
      \<less\>url C:\\Program Files (x86)\\TeXmacs\\bin\\gs.exe\<gtr\>
    </unfolded-io>

    <\input|Scheme] >
      \;
    </input>
  </session>

  <section|Various other binaries>

  <\session|scheme|default>
    <\unfolded-io|Scheme] >
      (use-modules (binary aspell))
    <|unfolded-io>
      (#1=(inlet '$tmapidoc $tmapidoc 'with-remote-identifier
      with-remote-identifier 'with-remote-get-user-name
      with-remote-get-user-name 'with-remote-get-user-pseudo
      with-remote-get-user-pseudo 'with-remote-search-user
      with-remote-search-user 'with-remote-search with-remote-search
      'with-remote-create-entry with-remote-create-entry
      'with-remote-get-entry with-remote-get-entry
      'with-remote-get-attributes with-remote-get-attributes
      'with-remote-get-field with-remote-get-field 'tm-call-back tm-call-back
      'with-remote-context with-remote-context ...))
    </unfolded-io>

    <\unfolded-io|Scheme] >
      (find-binary-aspell)
    <|unfolded-io>
      \<less\>url C:\\Users\\darcy\\scoop\\apps\\aspell\\current\\bin\\aspell.exe\<gtr\>
    </unfolded-io>

    <\unfolded-io|Scheme] >
      (use-modules (binary convert))
    <|unfolded-io>
      (#1=(inlet '$tmapidoc $tmapidoc 'with-remote-identifier
      with-remote-identifier 'with-remote-get-user-name
      with-remote-get-user-name 'with-remote-get-user-pseudo
      with-remote-get-user-pseudo 'with-remote-search-user
      with-remote-search-user 'with-remote-search with-remote-search
      'with-remote-create-entry with-remote-create-entry
      'with-remote-get-entry with-remote-get-entry
      'with-remote-get-attributes with-remote-get-attributes
      'with-remote-get-field with-remote-get-field 'tm-call-back tm-call-back
      'with-remote-context with-remote-context ...))
    </unfolded-io>

    <\unfolded-io|Scheme] >
      (find-binary-convert)
    <|unfolded-io>
      \<less\>url C:\\Program Files\\ImageMagick-7.1.1-Q16-HDRI\\convert.exe\<gtr\>
    </unfolded-io>

    <\unfolded-io|Scheme] >
      (find-binary () "convert")
    <|unfolded-io>
      \<less\>url {}\<gtr\>
    </unfolded-io>

    <\unfolded-io|Scheme] >
      (use-modules (binary inkscape))
    <|unfolded-io>
      (#1=(inlet '$tmapidoc $tmapidoc 'with-remote-identifier
      with-remote-identifier 'with-remote-get-user-name
      with-remote-get-user-name 'with-remote-get-user-pseudo
      with-remote-get-user-pseudo 'with-remote-search-user
      with-remote-search-user 'with-remote-search with-remote-search
      'with-remote-create-entry with-remote-create-entry
      'with-remote-get-entry with-remote-get-entry
      'with-remote-get-attributes with-remote-get-attributes
      'with-remote-get-field with-remote-get-field 'tm-call-back tm-call-back
      'with-remote-context with-remote-context ...))
    </unfolded-io>

    <\unfolded-io|Scheme] >
      (find-binary-inkscape)
    <|unfolded-io>
      \<less\>url C:\\Program Files\\Inkscape\\bin\\inkscape.exe\<gtr\>
    </unfolded-io>

    <\unfolded-io|Scheme] >
      (use-modules (binary python3))
    <|unfolded-io>
      (#1=(inlet '$tmapidoc $tmapidoc 'with-remote-identifier
      with-remote-identifier 'with-remote-get-user-name
      with-remote-get-user-name 'with-remote-get-user-pseudo
      with-remote-get-user-pseudo 'with-remote-search-user
      with-remote-search-user 'with-remote-search with-remote-search
      'with-remote-create-entry with-remote-create-entry
      'with-remote-get-entry with-remote-get-entry
      'with-remote-get-attributes with-remote-get-attributes
      'with-remote-get-field with-remote-get-field 'tm-call-back tm-call-back
      'with-remote-context with-remote-context ...))
    </unfolded-io>

    <\unfolded-io|Scheme] >
      (find-binary-python3)
    <|unfolded-io>
      \<less\>url C:\\Users\\darcy\\AppData\\Local\\Programs\\Python\\Python311\\python.exe\<gtr\>
    </unfolded-io>

    <\input|Scheme] >
      \;
    </input>
  </session>

  <section|Test on the switcher for the specified plugin>

  <\session|scheme|default>
    <\unfolded-io|Scheme] >
      (find-binary-python3)
    <|unfolded-io>
      \<less\>url /usr/bin/python3\<gtr\>
    </unfolded-io>

    <\unfolded-io|Scheme] >
      (get-preference "plugin:binary:python3")
    <|unfolded-io>
      default
    </unfolded-io>

    <\input|Scheme] >
      (set-preference "plugin:binary:python3" "/usr/bin/python3.11")
    </input>

    <\unfolded-io|Scheme] >
      (find-binary-python3)
    <|unfolded-io>
      \<less\>url /usr/bin/python3.11\<gtr\>
    </unfolded-io>

    <\input|Scheme] >
      (reset-preference "plugin:binary:python3")
    </input>

    <\unfolded-io|Scheme] >
      (find-binary-python3)
    <|unfolded-io>
      \<less\>url /usr/bin/python3\<gtr\>
    </unfolded-io>

    <\input|Scheme] >
      (set-preference "plugin:binary:python3" "off")
    </input>

    <\unfolded-io|Scheme] >
      (find-binary-python3)
    <|unfolded-io>
      \<less\>url {}\<gtr\>
    </unfolded-io>

    <\input|Scheme] >
      (set-preference "plugin:binary:python3" "candidates-only")
    </input>

    <\unfolded-io|Scheme] >
      (find-binary-python3)
    <|unfolded-io>
      \<less\>url /usr/bin/python3\<gtr\>
    </unfolded-io>

    <\input|Scheme] >
      \;
    </input>
  </session>

  <section|Test on the switcher for all binary plugins>

  <\session|scheme|default>
    <\unfolded-io|Scheme] >
      (find-binary-python3)
    <|unfolded-io>
      \<less\>url /usr/bin/python3.11\<gtr\>
    </unfolded-io>

    <\unfolded-io|Scheme] >
      (get-preference "plugin:binary")
    <|unfolded-io>
      default
    </unfolded-io>

    <\input|Scheme] >
      (set-preference "plugin:binary" "off")
    </input>

    <\unfolded-io|Scheme] >
      (get-preference "plugin:binary")
    <|unfolded-io>
      off
    </unfolded-io>

    <\unfolded-io|Scheme] >
      (url-none? (find-binary-python3))
    <|unfolded-io>
      #t
    </unfolded-io>

    <\input|Scheme] >
      (reset-preference "plugin:binary")
    </input>

    <\input|Scheme] >
      \;
    </input>
  </session>

  <section|Version>

  <\session|scheme|default>
    <\folded-io|Scheme] >
      (use-modules (binary aspell))
    <|folded-io>
      (#1=(inlet 'supports-slidemove? supports-slidemove? 'supports-python?
      supports-python? 'python-launcher python-launcher 'python-utf8-command
      python-utf8-command 'python-serialize python-serialize
      'scala-snippet-\<gtr\>texmacs scala-snippet-\<gtr\>texmacs
      'scala-\<gtr\>texmacs scala-\<gtr\>texmacs 'texmacs-\<gtr\>scala
      texmacs-\<gtr\>scala 'python-snippet-\<gtr\>texmacs
      python-snippet-\<gtr\>texmacs 'python-\<gtr\>texmacs
      python-\<gtr\>texmacs 'texmacs-\<gtr\>python texmacs-\<gtr\>python
      'julia-snippet-\<gtr\>texmacs julia-snippet-\<gtr\>texmacs ...))
    </folded-io>

    <\folded-io|Scheme] >
      (version-binary-aspell)
    <|folded-io>
      @(#) International Ispell Version 3.1.20 (but really Aspell 0.60.8.1)
    </folded-io>

    <\folded-io|Scheme] >
      (use-modules (binary convert))
    <|folded-io>
      (#1=(inlet 'supports-slidemove? supports-slidemove? 'supports-python?
      supports-python? 'python-launcher python-launcher 'python-utf8-command
      python-utf8-command 'python-serialize python-serialize
      'scala-snippet-\<gtr\>texmacs scala-snippet-\<gtr\>texmacs
      'scala-\<gtr\>texmacs scala-\<gtr\>texmacs 'texmacs-\<gtr\>scala
      texmacs-\<gtr\>scala 'python-snippet-\<gtr\>texmacs
      python-snippet-\<gtr\>texmacs 'python-\<gtr\>texmacs
      python-\<gtr\>texmacs 'texmacs-\<gtr\>python texmacs-\<gtr\>python
      'julia-snippet-\<gtr\>texmacs julia-snippet-\<gtr\>texmacs ...))
    </folded-io>

    <\input|Scheme] >
      (version-binary-convert)
    </input>

    <\unfolded-io|Scheme] >
      (use-modules (binary hunspell))
    <|unfolded-io>
      (#1=(inlet 'supports-slidemove? supports-slidemove? 'supports-python?
      supports-python? 'python-launcher python-launcher 'python-utf8-command
      python-utf8-command 'python-serialize python-serialize
      'scala-snippet-\<gtr\>texmacs scala-snippet-\<gtr\>texmacs
      'scala-\<gtr\>texmacs scala-\<gtr\>texmacs 'texmacs-\<gtr\>scala
      texmacs-\<gtr\>scala 'python-snippet-\<gtr\>texmacs
      python-snippet-\<gtr\>texmacs 'python-\<gtr\>texmacs
      python-\<gtr\>texmacs 'texmacs-\<gtr\>python texmacs-\<gtr\>python
      'julia-snippet-\<gtr\>texmacs julia-snippet-\<gtr\>texmacs ...))
    </unfolded-io>

    <\unfolded-io|Scheme] >
      (version-binary-hunspell)
    <|unfolded-io>
      @(#) International Ispell Version 3.2.06 (but really Hunspell 1.7.2)
    </unfolded-io>

    <\folded-io|Scheme] >
      (use-modules (binary identify))
    <|folded-io>
      (#1=(inlet 'supports-slidemove? supports-slidemove? 'supports-python?
      supports-python? 'python-launcher python-launcher 'python-utf8-command
      python-utf8-command 'python-serialize python-serialize
      'scala-snippet-\<gtr\>texmacs scala-snippet-\<gtr\>texmacs
      'scala-\<gtr\>texmacs scala-\<gtr\>texmacs 'texmacs-\<gtr\>scala
      texmacs-\<gtr\>scala 'python-snippet-\<gtr\>texmacs
      python-snippet-\<gtr\>texmacs 'python-\<gtr\>texmacs
      python-\<gtr\>texmacs 'texmacs-\<gtr\>python texmacs-\<gtr\>python
      'julia-snippet-\<gtr\>texmacs julia-snippet-\<gtr\>texmacs ...))
    </folded-io>

    <\input|Scheme] >
      (version-binary-identify)
    </input>

    <\folded-io|Scheme] >
      (use-modules (binary inkscape))
    <|folded-io>
      (#1=(inlet 'supports-slidemove? supports-slidemove? 'supports-python?
      supports-python? 'python-launcher python-launcher 'python-utf8-command
      python-utf8-command 'python-serialize python-serialize
      'scala-snippet-\<gtr\>texmacs scala-snippet-\<gtr\>texmacs
      'scala-\<gtr\>texmacs scala-\<gtr\>texmacs 'texmacs-\<gtr\>scala
      texmacs-\<gtr\>scala 'python-snippet-\<gtr\>texmacs
      python-snippet-\<gtr\>texmacs 'python-\<gtr\>texmacs
      python-\<gtr\>texmacs 'texmacs-\<gtr\>python texmacs-\<gtr\>python
      'julia-snippet-\<gtr\>texmacs julia-snippet-\<gtr\>texmacs ...))
    </folded-io>

    <\input|Scheme] >
      (version-binary-inkscape)
    </input>

    <\input|Scheme] >
      (version-binary-pdftocairo)
    </input>

    <\folded-io|Scheme] >
      (use-modules (binary rsvg-convert))
    <|folded-io>
      (#1=(inlet 'supports-slidemove? supports-slidemove? 'supports-python?
      supports-python? 'python-launcher python-launcher 'python-utf8-command
      python-utf8-command 'python-serialize python-serialize
      'scala-snippet-\<gtr\>texmacs scala-snippet-\<gtr\>texmacs
      'scala-\<gtr\>texmacs scala-\<gtr\>texmacs 'texmacs-\<gtr\>scala
      texmacs-\<gtr\>scala 'python-snippet-\<gtr\>texmacs
      python-snippet-\<gtr\>texmacs 'python-\<gtr\>texmacs
      python-\<gtr\>texmacs 'texmacs-\<gtr\>python texmacs-\<gtr\>python
      'julia-snippet-\<gtr\>texmacs julia-snippet-\<gtr\>texmacs ...))
    </folded-io>

    <\input|Scheme] >
      (version-binary-rsvg-convert)
    </input>

    <\input|Scheme] >
      (version-binary-octave)
    </input>

    <\folded-io|Scheme] >
      (version-binary-maxima)
    <|folded-io>
      Maxima 5.46.0
    </folded-io>

    <\input|Scheme] >
      \;
    </input>
  </session>
</body>

<\initial>
  <\collection>
    <associate|page-screen-margin|false>
    <associate|save-aux|false>
  </collection>
</initial>
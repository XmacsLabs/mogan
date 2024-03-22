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
      \<less\>url /opt/homebrew/Cellar/ghostscript/10.02.1/bin/gs\<gtr\>
    </unfolded-io>

    <\input|Scheme] >
      (set-preference "plugin:binary:gs" "/bin/gs")
    </input>

    <\unfolded-io|Scheme] >
      (find-binary-gs)
    <|unfolded-io>
      \<less\>url /opt/homebrew/Cellar/ghostscript/10.02.1/bin/gs\<gtr\>
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
</body>

<\initial>
  <\collection>
    <associate|page-screen-margin|false>
  </collection>
</initial>

<\references>
  <\collection>
    <associate|auto-1|<tuple|1|?>>
    <associate|auto-2|<tuple|2|?>>
    <associate|auto-3|<tuple|3|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|toc>
      <vspace*|1fn><with|font-series|<quote|bold>|math-font-series|<quote|bold>|1<space|2spc>Test
      on macOS> <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-1><vspace|0.5fn>

      <vspace*|1fn><with|font-series|<quote|bold>|math-font-series|<quote|bold>|2<space|2spc>Test
      on Windows> <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-2><vspace|0.5fn>

      <vspace*|1fn><with|font-series|<quote|bold>|math-font-series|<quote|bold>|3<space|2spc>Various
      other binaries> <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-3><vspace|0.5fn>
    </associate>
  </collection>
</auxiliary>
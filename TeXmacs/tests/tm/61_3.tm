<TeXmacs|2.1.2>

<style|<tuple|generic|no-page-numbers|british>>

<\body>
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
</body>

<\initial>
  <\collection>
    <associate|page-screen-margin|false>
  </collection>
</initial>
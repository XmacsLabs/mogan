<TeXmacs|2.1.2>

<style|<tuple|generic|no-page-numbers|chinese>>

<\body>
  <\session|scheme|default>
    <\unfolded-io|Scheme] >
      (use-modules (binary gnuplot))
    <|unfolded-io>
      (#1=(inlet 'supports-slidemove? supports-slidemove? 'supports-python?
      supports-python? 'all-python-launchers all-python-launchers
      'conda-launchers conda-launchers 'conda-launcher conda-launcher
      'python-launcher python-launcher 'python-utf8-command
      python-utf8-command 'python-serialize python-serialize
      'scala-snippet-\<gtr\>texmacs scala-snippet-\<gtr\>texmacs
      'scala-\<gtr\>texmacs scala-\<gtr\>texmacs 'texmacs-\<gtr\>scala
      texmacs-\<gtr\>scala 'python-snippet-\<gtr\>texmacs
      python-snippet-\<gtr\>texmacs ...))
    </unfolded-io>

    <\unfolded-io|Scheme] >
      (find-binary-gnuplot)
    <|unfolded-io>
      \<less\>url /opt/homebrew/bin/gnuplot\<gtr\>
    </unfolded-io>

    <\unfolded-io|Scheme] >
      (has-binary-gnuplot?)
    <|unfolded-io>
      #t
    </unfolded-io>

    <\unfolded-io|Scheme] >
      (version-binary-gnuplot)
    <|unfolded-io>
      gnuplot 6.0 patchlevel 0
    </unfolded-io>

    <\input|Scheme] >
      \;
    </input>
  </session>
</body>

<\initial>
  <\collection>
    <associate|page-medium|paper>
    <associate|page-screen-margin|false>
  </collection>
</initial>
<TeXmacs|2.1.2>

<style|<tuple|generic|no-page-numbers|chinese>>

<\body>
  <section|Windows>

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
      \<less\>url C:\\Program Files\\gnuplot\\bin\\gnuplot.exe\<gtr\>
    </unfolded-io>

    <\unfolded-io|Scheme] >
      (version-binary-gnuplot)
    <|unfolded-io>
      gnuplot 5.4 patchlevel 8
    </unfolded-io>

    <\input|Scheme] >
      \;
    </input>
  </session>

  <section|macOS>

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

<\references>
  <\collection>
    <associate|auto-1|<tuple|1|1>>
    <associate|auto-2|<tuple|2|1>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|toc>
      <vspace*|1fn><with|font-series|<quote|bold>|math-font-series|<quote|bold>|1<space|2spc>Windows>
      <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-1><vspace|0.5fn>

      <vspace*|1fn><with|font-series|<quote|bold>|math-font-series|<quote|bold>|2<space|2spc>macOS>
      <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-2><vspace|0.5fn>
    </associate>
  </collection>
</auxiliary>
<TeXmacs|2.1.2>

<style|generic>

<\body>
  <section|Python 3>

  <\session|scheme|default>
    <\unfolded-io|Scheme] >
      (use-modules (binary python3))
    <|unfolded-io>
      (#1=(inlet 'scala-snippet-\<gtr\>texmacs scala-snippet-\<gtr\>texmacs
      'scala-\<gtr\>texmacs scala-\<gtr\>texmacs 'texmacs-\<gtr\>scala
      texmacs-\<gtr\>scala 'python-snippet-\<gtr\>texmacs
      python-snippet-\<gtr\>texmacs 'python-\<gtr\>texmacs
      python-\<gtr\>texmacs 'texmacs-\<gtr\>python texmacs-\<gtr\>python
      'julia-snippet-\<gtr\>texmacs julia-snippet-\<gtr\>texmacs
      'julia-\<gtr\>texmacs julia-\<gtr\>texmacs 'texmacs-\<gtr\>julia
      texmacs-\<gtr\>julia 'javascript-snippet-\<gtr\>texmacs
      javascript-snippet-\<gtr\>texmacs 'javascript-\<gtr\>texmacs
      javascript-\<gtr\>texmacs 'texmacs-\<gtr\>javascript
      texmacs-\<gtr\>javascript ...))
    </unfolded-io>

    <\unfolded-io|Scheme] >
      (find-binary-python3)
    <|unfolded-io>
      \<less\>url /usr/bin/python3\<gtr\>
    </unfolded-io>

    <\unfolded-io|Scheme] >
      (has-binary-python3?)
    <|unfolded-io>
      #t
    </unfolded-io>

    <\input|Scheme] >
      \;
    </input>
  </session>

  <section|Conda>

  <\session|scheme|default>
    <\unfolded-io|Scheme] >
      (use-modules (binary conda))
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
      (find-binary-conda)
    <|unfolded-io>
      \<less\>url /home/da/miniconda3/bin/conda\<gtr\>
    </unfolded-io>

    <\unfolded-io|Scheme] >
      (version-binary-conda)
    <|unfolded-io>
      conda 24.1.2
    </unfolded-io>

    <\input|Scheme] >
      \;
    </input>
  </session>
</body>

<initial|<\collection>
</collection>>

<\references>
  <\collection>
    <associate|auto-1|<tuple|1|?>>
    <associate|auto-2|<tuple|2|?>>
  </collection>
</references>
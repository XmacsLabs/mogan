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
      \<less\>url C:\\Users\\darcy\\miniconda3\\Scripts\\conda.exe\<gtr\>
    </unfolded-io>

    <\input|Scheme] >
      (version-binary-conda)
    </input>

    <\unfolded-io|Scheme] >
      (conda-env-python-list)
    <|unfolded-io>
      (\<less\>url C:\\Users\\darcy\\.conda\\envs\\myenvironment\\python.exe\<gtr\>)
    </unfolded-io>

    <\input|Scheme] >
      \;
    </input>
  </session>
</body>

<\initial>
  <\collection>
    <associate|save-aux|false>
  </collection>
</initial>
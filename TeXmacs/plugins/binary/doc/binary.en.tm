<TeXmacs|2.1.2>

<style|<tuple|tmdoc|british>>

<\body>
  <tmdoc-title|Binary Plugin>

  Binary plugins are a type of plugins that are primarily used for:

  <\itemize>
    <item>Locating the position of executable files and determining whether
    they exist.

    <item>Invoking executable files based on actual needs, and encapsulating
    the preprocessing of inputs and post-processing logic of outputs.
  </itemize>

  For the complete documentation, please refer to the <hlink|Binary Plugin
  page|https://mogan.app/guide/plugins.html> on our official website. This
  document will demonstrate how to configure binary plugins through a Scheme
  session in the form of a Scheme session. Taking <shell|python3> as an
  example, you can replace <shell|python3> with other binary plugins (such as
  <shell|gs>) through the <menu|Edit|Replace> function. The Scheme session
  described in this document will still be valid.

  <section*|Location>

  <\session|scheme|default>
    <\folded-io|Scheme] >
      (use-modules (binary python3))
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

  <section*|Customization>

  Taking the Python plugin as an example, insert Python session via
  <menu|Insert|Fold|Executable|Python>:

  <\session|python|default>
    <\output>
      Python 3.12.2 [/usr/bin/python3]

      Python plugin for TeXmacs.

      Please see the documentation in Help -\<gtr\> Plugins -\<gtr\> Python
    </output>

    <\input>
      \<gtr\>\<gtr\>\<gtr\>\ 
    <|input>
      \;
    </input>
  </session>

  \;

  The Python interpreter used by the default Python session is determined by
  <scm|(find-binary-python3)>.

  <\session|scheme|default>
    <\unfolded-io|Scheme] >
      (find-binary-python3)
    <|unfolded-io>
      \<less\>url /usr/bin/python3\<gtr\>
    </unfolded-io>

    <\unfolded-io|Scheme] >
      (version-binary-python3)
    <|unfolded-io>
      Python 3.12.2
    </unfolded-io>

    <\input|Scheme] >
      (set-preference "plugin:binary:python3" "/usr/bin/python3.11")
    </input>

    <\unfolded-io|Scheme] >
      (find-binary-python3)
    <|unfolded-io>
      \<less\>url /usr/bin/python3.11\<gtr\>
    </unfolded-io>

    <\unfolded-io|Scheme] >
      (version-binary-python3)
    <|unfolded-io>
      Python 3.11.8
    </unfolded-io>

    <\input|Scheme] >
      \;
    </input>
  </session>

  \;

  We have already set the default binary path for python3 to
  <scm|"/usr/bin/python3.11">. Now, by restarting Mogan, you can insert a
  Python session for Python 3.11.

  <\session|python|default>
    <\output>
      Python 3.11.8 [/usr/bin/python3.11]

      Python plugin for TeXmacs.

      Please see the documentation in Help -\<gtr\> Plugins -\<gtr\> Python
    </output>

    <\input>
      \<gtr\>\<gtr\>\<gtr\>\ 
    <|input>
      \;
    </input>
  </session>

  <section*|Security>

  Given the potential security risks associated with calling external
  commands, it is also possible to completely disable binary plugins:

  <\session|scheme|default>
    <\unfolded-io|Scheme] >
      (get-preference "plugin:binary")
    <|unfolded-io>
      default
    </unfolded-io>

    <\unfolded-io|Scheme] >
      (find-binary-python3)
    <|unfolded-io>
      \<less\>url /usr/bin/python3.11\<gtr\>
    </unfolded-io>

    <\input|Scheme] >
      (set-preference "plugin:binary" "off")
    </input>

    <\unfolded-io|Scheme] >
      (find-binary-python3)
    <|unfolded-io>
      \<less\>url {}\<gtr\>
    </unfolded-io>

    <\unfolded-io|Scheme] >
      (has-binary-python3?)
    <|unfolded-io>
      #f
    </unfolded-io>

    <\input|Scheme] >
      (reset-preference "plugin:binary")
    </input>

    <\unfolded-io|Scheme] >
      (get-preference "plugin:binary")
    <|unfolded-io>
      default
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
</body>

<\initial>
  <\collection>
    <associate|page-medium|papyrus>
    <associate|page-screen-margin|true>
  </collection>
</initial>
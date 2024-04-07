<TeXmacs|2.1.2>

<style|<tuple|tmdoc|chinese|python>>

<\body>
  <tmdoc-title|\<#4E8C\>\<#8FDB\>\<#5236\>\<#63D2\>\<#4EF6\>>

  \<#4E8C\>\<#8FDB\>\<#5236\>\<#63D2\>\<#4EF6\>\<#662F\>\<#4E00\>\<#7C7B\>\<#63D2\>\<#4EF6\>\<#FF0C\>\<#4E3B\>\<#8981\>\<#7528\>\<#4E8E\>\<#FF1A\>

  <\itemize>
    <item>\<#5B9A\>\<#4F4D\>\<#53EF\>\<#6267\>\<#884C\>\<#6587\>\<#4EF6\>\<#7684\>\<#4F4D\>\<#7F6E\>\<#5E76\>\<#5224\>\<#65AD\>\<#53EF\>\<#6267\>\<#884C\>\<#6587\>\<#4EF6\>\<#662F\>\<#5426\>\<#5B58\>\<#5728\>

    <item>\<#4F9D\>\<#636E\>\<#5B9E\>\<#9645\>\<#9700\>\<#6C42\>\<#8C03\>\<#7528\>\<#53EF\>\<#6267\>\<#884C\>\<#6587\>\<#4EF6\>\<#FF0C\>\<#5E76\>\<#5C01\>\<#88C5\>\<#8F93\>\<#5165\>\<#7684\>\<#9884\>\<#5904\>\<#7406\>\<#548C\>\<#8F93\>\<#51FA\>\<#7684\>\<#540E\>\<#7F6E\>\<#5904\>\<#7406\>\<#903B\>\<#8F91\>
  </itemize>

  \<#5B8C\>\<#6574\>\<#7684\>\<#6587\>\<#6863\>\<#8BF7\>\<#9605\>\<#8BFB\>\<#5B98\>\<#7F51\>\<#7684\><hlink|\<#4E8C\>\<#8FDB\>\<#5236\>\<#63D2\>\<#4EF6\>|https://mogan.app/zh/guide/plugins.html>\<#9875\>\<#9762\>\<#3002\>\<#672C\>\<#6587\>\<#6863\>\<#5C06\>\<#4EE5\>Scheme\<#4F1A\>\<#8BDD\>\<#7684\>\<#5F62\>\<#5F0F\>\<#FF0C\>\<#6F14\>\<#793A\>\<#5982\>\<#4F55\>\<#7528\>Scheme\<#4F1A\>\<#8BDD\>\<#914D\>\<#7F6E\>\<#4E8C\>\<#8FDB\>\<#5236\>\<#63D2\>\<#4EF6\>\<#3002\>\<#672C\>\<#6587\>\<#4EE5\><shell|python3>\<#4E3A\>\<#4F8B\>\<#FF0C\>\<#901A\>\<#8FC7\><menu|Insert|Replace>\<#53EF\>\<#4EE5\>\<#5C06\><shell|python3>\<#66FF\>\<#6362\>\<#4E3A\>\<#5176\>\<#4ED6\>\<#7684\>\<#4E8C\>\<#8FDB\>\<#5236\>\<#63D2\>\<#4EF6\>\<#FF08\>\<#6BD4\>\<#5982\><shell|gs>\<#FF09\>\<#FF0C\>\<#672C\>\<#6587\>\<#7684\>Scheme\<#4F1A\>\<#8BDD\>\<#4ECD\>\<#65E7\>\<#751F\>\<#6548\>\<#3002\>

  <section*|\<#5B9A\>\<#4F4D\>>

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

  <section*|\<#5B9A\>\<#5236\>>

  \<#4EE5\>Python\<#63D2\>\<#4EF6\>\<#4E3A\>\<#4F8B\>\<#FF0C\>\<#53EF\>\<#4EE5\>\<#901A\>\<#8FC7\><menu|Insert|Fold|Executable|Python>\<#63D2\>\<#5165\>Python\<#4F1A\>\<#8BDD\>\<#FF1A\>

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

  \<#9ED8\>\<#8BA4\>\<#7684\>Python\<#4F1A\>\<#8BDD\>\<#4F7F\>\<#7528\>\<#7684\>Python\<#89E3\>\<#91CA\>\<#5668\>\<#662F\>\<#7531\><scm|(find-binary-python3)>\<#51B3\>\<#5B9A\>\<#7684\>\<#3002\>

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

  \<#6211\>\<#4EEC\>\<#5DF2\>\<#7ECF\>\<#5C06\><shell|python3>\<#7684\>\<#9ED8\>\<#8BA4\>\<#4E8C\>\<#8FDB\>\<#5236\>\<#8DEF\>\<#5F84\>\<#8BBE\>\<#7F6E\>\<#4E3A\><scm|"/usr/bin/python3.11">\<#3002\>\<#6B64\>\<#65F6\>\<#91CD\>\<#542F\>\<#58A8\>\<#5E72\>\<#FF0C\>\<#5C31\>\<#53EF\>\<#4EE5\>\<#63D2\>\<#5165\>Python
  3.11\<#7684\>Python\<#4F1A\>\<#8BDD\>\<#FF1A\>

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

  <section*|\<#5B89\>\<#5168\>>

  \<#8003\>\<#8651\>\<#5230\>\<#8C03\>\<#7528\>\<#5916\>\<#90E8\>\<#547D\>\<#4EE4\>\<#5B58\>\<#5728\>\<#7684\>\<#5B89\>\<#5168\>\<#9690\>\<#60A3\>\<#FF0C\>\<#4E5F\>\<#53EF\>\<#4EE5\>\<#5B8C\>\<#5168\>\<#7981\>\<#7528\>\<#4E8C\>\<#8FDB\>\<#5236\>\<#63D2\>\<#4EF6\>\<#FF1A\>

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
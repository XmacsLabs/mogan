<TeXmacs|2.1.2>

<style|<tuple|generic|no-page-numbers|british>>

<\body>
  How to reproduce the bug:

  <\session|octave|default>
    <\output>
      GNU Octave (8.4.0) Session in GNU TeXmacs

      Welcome to star and fork it at https://github.com/texmacs/octave
    </output>

    <\unfolded-io>
      \<gtr\>\<gtr\>\ 
    <|unfolded-io>
      ones (1)
    <|unfolded-io>
      <with|mode|math|1>
    </unfolded-io>

    <\input>
      \<gtr\>\<gtr\>\ 
    <|input>
      ones (0)
    </input>
  </session>

  The scheme data:

  <\session|octave|default>
    <\unfolded-io>
      \<gtr\>\<gtr\>\ 
    <|unfolded-io>
      mat2scm (ones (0))
    <|unfolded-io>
      (with "mode" "math" "math-display" "true" (matrix (tformat (table))))
    </unfolded-io>

    <\input>
      \<gtr\>\<gtr\>\ 
    <|input>
      \;
    </input>
  </session>

  Flush scheme data to reproduce the bug:

  <\session|octave|default>
    <\input>
      \<gtr\>\<gtr\>\ 
    <|input>
      flush_scheme ("(matrix (tformat (table)))")
    </input>
  </session>
</body>

<\initial>
  <\collection>
    <associate|page-medium|beamer>
    <associate|page-screen-margin|false>
  </collection>
</initial>
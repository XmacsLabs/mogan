<TeXmacs|2.1.2>

<style|<tuple|generic|no-page-numbers|british>>

<\body>
  <section|How to reproduce the bug>

  How to reproduce the bug:

  <\session|octave|default>
    <\output>
      GNU Octave (8.4.0) Session in GNU TeXmacs

      Welcome to star and fork it at https://github.com/texmacs/octave
    </output>

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

    <\input|Octave] >
      flush_scheme ("(tformat (table))")
    </input>
  </session>

  <section|How to test>

  <\session|octave|default>
    <\unfolded-io>
      \<gtr\>\<gtr\>\ 
    <|unfolded-io>
      ones(0)
    <|unfolded-io>
      <stack|<tformat|<table|<row|<cell|[](0x0)>>>>>
    </unfolded-io>

    <\unfolded-io>
      \<gtr\>\<gtr\>\ 
    <|unfolded-io>
      ones(1)
    <|unfolded-io>
      <with|mode|math|1>
    </unfolded-io>

    <\unfolded-io>
      \<gtr\>\<gtr\>\ 
    <|unfolded-io>
      ones(2)
    <|unfolded-io>
      <with|mode|math|math-display|true|<matrix|<tformat|<table|<row|<cell|<with|mode|math|1>>|<cell|<with|mode|math|1>>>|<row|<cell|<with|mode|math|1>>|<cell|<with|mode|math|1>>>>>>>
    </unfolded-io>

    <\input>
      \<gtr\>\<gtr\>\ 
    <|input>
      \;
    </input>
  </session>

  \;
</body>

<\initial>
  <\collection>
    <associate|page-medium|beamer>
    <associate|page-screen-margin|false>
  </collection>
</initial>

<\references>
  <\collection>
    <associate|auto-1|<tuple|1|?>>
    <associate|auto-2|<tuple|2|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|toc>
      <vspace*|1fn><with|font-series|<quote|bold>|math-font-series|<quote|bold>|1<space|2spc>How
      to reproduce the bug> <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-1><vspace|0.5fn>

      <vspace*|1fn><with|font-series|<quote|bold>|math-font-series|<quote|bold>|2<space|2spc>How
      to test> <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-2><vspace|0.5fn>
    </associate>
  </collection>
</auxiliary>
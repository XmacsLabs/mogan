<TeXmacs|2.1.2>

<style|<tuple|generic|no-page-numbers|chinese>>

<\body>
  \;

  <image|../PDF/38_2.pdf||||>

  <section|Ghostscript Binary Plugin >

  <slink|$TEXMACS_PATH/progs/plugins/binary/gs.scm>

  <\session|scheme|default>
    <\unfolded-io|Scheme] >
      (find-binary-gs)
    <|unfolded-io>
      \<less\>url /opt/homebrew/Cellar/ghostscript/10.02.1/bin/gs\<gtr\>
    </unfolded-io>

    <\unfolded-io|Scheme] >
      (has-binary-gs?)
    <|unfolded-io>
      #t
    </unfolded-io>

    <\input|Scheme] >
      \;
    </input>
  </session>

  <section|Convert Binary Plugin>

  <slink|$TEXMACS_PATH/progs/plugins/binary/convert.scm>

  <\session|scheme|default>
    <\unfolded-io|Scheme] >
      (find-binary-convert)
    <|unfolded-io>
      \<less\>url /opt/homebrew/bin/convert\<gtr\>
    </unfolded-io>

    <\unfolded-io|Scheme] >
      (has-binary-convert?)
    <|unfolded-io>
      #t
    </unfolded-io>

    <\input|Scheme] >
      \;
    </input>
  </session>

  <section|pdftocairo Binary Plugin>

  <slink|$TEXMACS_PATH/progs/plugins/binary/pdftocairo.scm>

  <\session|scheme|default>
    <\unfolded-io|Scheme] >
      (find-binary-pdftocairo)
    <|unfolded-io>
      \<less\>url /opt/homebrew/bin/pdftocairo\<gtr\>
    </unfolded-io>

    <\unfolded-io|Scheme] >
      (has-binary-pdftocairo?)
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
    <associate|page-medium|paper>
    <associate|page-screen-margin|false>
  </collection>
</initial>

<\references>
  <\collection>
    <associate|auto-1|<tuple|1|1>>
    <associate|auto-2|<tuple|2|1>>
    <associate|auto-3|<tuple|3|1>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|toc>
      <vspace*|1fn><with|font-series|<quote|bold>|math-font-series|<quote|bold>|1<space|2spc>Ghostscript
      Binary Plugin > <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-1><vspace|0.5fn>

      <vspace*|1fn><with|font-series|<quote|bold>|math-font-series|<quote|bold>|2<space|2spc>Convert
      Binary Plugin> <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-2><vspace|0.5fn>

      <vspace*|1fn><with|font-series|<quote|bold>|math-font-series|<quote|bold>|3<space|2spc>pdftocairo
      Binary Plugin> <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-3><vspace|0.5fn>
    </associate>
  </collection>
</auxiliary>
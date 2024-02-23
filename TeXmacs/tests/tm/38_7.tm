<TeXmacs|2.1.2>

<style|<tuple|generic|no-page-numbers|chinese>>

<\body>
  small svg:

  <image|../../misc/images/new-mogan.svg|100pt|||>

  big svg:

  <image|../../misc/images/new-mogan.svg|213pt|213pt||>

  <section|rsvg-convert Binary Plugin >

  <slink|$TEXMACS_PATH/plugins/binary/progs/binary/rsvg-convert.scm>

  <\session|scheme|default>
    <\unfolded-io|Scheme] >
      (find-binary-rsvg-convert)
    <|unfolded-io>
      \<less\>url /opt/homebrew/bin/rsvg-convert\<gtr\>
    </unfolded-io>

    <\unfolded-io|Scheme] >
      (has-binary-rsvg-convert?)
    <|unfolded-io>
      #t
    </unfolded-io>

    <\input|Scheme] >
      \;
    </input>
  </session>

  <section|inkscape Binary Plugin>

  <slink|$TEXMACS_PATH/plugins/binary/progs/binary/inkscape.scm>

  <\session|scheme|default>
    <\unfolded-io|Scheme] >
      (find-binary-inkscape)
    <|unfolded-io>
      \<less\>url /Applications/Inkscape.app/Contents/MacOS/inkscape\<gtr\>
    </unfolded-io>

    <\unfolded-io|Scheme] >
      (has-binary-inkscape?)
    <|unfolded-io>
      #t
    </unfolded-io>

    <\input|Scheme] >
      \;
    </input>
  </session>

  \;
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
      <vspace*|1fn><with|font-series|<quote|bold>|math-font-series|<quote|bold>|1<space|2spc>rsvg-convert
      Binary Plugin > <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-1><vspace|0.5fn>

      <vspace*|1fn><with|font-series|<quote|bold>|math-font-series|<quote|bold>|2<space|2spc>inkscape
      Binary Plugin> <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-2><vspace|0.5fn>
    </associate>
  </collection>
</auxiliary>
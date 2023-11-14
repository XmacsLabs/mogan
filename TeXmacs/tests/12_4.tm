<TeXmacs|2.1.2>

<style|generic>

<\body>
  <doc-data|<doc-title|test>|<doc-author|<author-data|<author-name|Someone>|<\author-affiliation>
    today
  </author-affiliation>>>>

  <section|test>

  <cite|1|2>

  <section|Ref>

  <\bibliography|bib|tm-plain|12_4>
    <\bib-list|2>
      <bibitem*|1><label|bib-2>A<nbsp>Bad Guy. <newblock>Work 2.
      <newblock><with|font-shape|italic|Nat. Phys.>, 18(8):925-930, Aug
      2022.<newblock>

      <bibitem*|2><label|bib-1>A<nbsp>Good Guy. <newblock>Work 1.
      <newblock><with|font-shape|italic|PRX Quantum>, 3:20308, Apr
      2022.<newblock>
    </bib-list>
  </bibliography>
</body>

<\initial>
  <\collection>
    <associate|page-medium|paper>
  </collection>
</initial>

<\references>
  <\collection>
    <associate|auto-1|<tuple|1|1|..\\..\\..\\..\\Downloads\\test\\test\\test.tm>>
    <associate|auto-2|<tuple|2|1|..\\..\\..\\..\\Downloads\\test\\test\\test.tm>>
    <associate|auto-3|<tuple|2|1|..\\..\\..\\..\\Downloads\\test\\test\\test.tm>>
    <associate|bib-1|<tuple|2|1|..\\..\\..\\..\\Downloads\\test\\test\\test.tm>>
    <associate|bib-2|<tuple|1|1|..\\..\\..\\..\\Downloads\\test\\test\\test.tm>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|bib>
      1

      2
    </associate>
    <\associate|toc>
      <vspace*|1fn><with|font-series|<quote|bold>|math-font-series|<quote|bold>|1<space|2spc>test>
      <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-1><vspace|0.5fn>

      <vspace*|1fn><with|font-series|<quote|bold>|math-font-series|<quote|bold>|2<space|2spc>Ref>
      <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-2><vspace|0.5fn>

      <vspace*|1fn><with|font-series|<quote|bold>|math-font-series|<quote|bold>|Bibliography>
      <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-3><vspace|0.5fn>
    </associate>
  </collection>
</auxiliary>
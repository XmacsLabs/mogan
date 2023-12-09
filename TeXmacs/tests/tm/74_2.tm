<TeXmacs|2.1.2>

<style|<tuple|amsart|preview-ref|number-long-article|smart-ref>>

<\body>
  <\hide-preamble>
    <comment|Math fonts>

    <assign|ms|<macro|body|<with|math-font|cal*|<arg|body>>>>

    <assign|mc|<macro|body|<with|math-font|cal|<arg|body>>>>

    <assign|mf|<macro|body|<with|math-font|Euler|<arg|body>>>>

    <assign|mbb|<macro|body|<with|math-font|Bbb|<arg|body>>>>

    <assign|mbf|<macro|body|<with|font-series|bold|<arg|body>>>>

    \;

    <comment|Local to this file>

    <comment|Strichartz exponents>

    <assign|sp|<macro|<mf|p>>>

    <assign|sq|<macro|<mf|q>>>

    <comment|A collection of Strichartz exponents>

    <assign|Stri|<macro|<mf|S>>>

    <comment|Anisotropic exponents>

    <assign|ana|<macro|<mf|a>>>

    <assign|anb|<macro|<mf|b>>>

    <comment|A collection of Anisotropic estimates exponents>

    <assign|Ani|<macro|<mf|A>>>

    <assign|AniMax|<macro|<mf|A><rsup|2,\<infty\>>>>

    <assign|AniLS|<macro|<mf|A><rsup|\<infty\>,2>>>

    <comment|Generalized elliptic operator>

    <assign|Lap|<macro|<ms|L>>>

    <comment|Hessian>

    <assign|D2|<macro|<math-up|D><rsup|2>>>

    <comment|Temporary>

    <assign|Aa|<macro|<mbb|A>>>

    <assign|Bb|<macro|<mbb|B>>>

    \;

    <assign|LP|<macro|<math-up|P>>>

    <assign|UQ|<macro|<math-up|Q>>>

    \;
  </hide-preamble>

  <section|Nonlinear estimates><label|sec:nonlin>

  \;

  \;

  <\lemma>
    \;

    <\equation*>
      <tabular*|<tformat|<twith|table-valign|c>|<cwith|2|-1|1|-1|cell-valign|c>|<cwith|7|7|1|2|cell-valign|c>|<cwith|9|9|1|2|cell-valign|c>|<cwith|11|15|1|2|cell-valign|c>|<cwith|25|26|1|2|cell-valign|c>|<cwith|21|23|1|2|cell-valign|c>|<cwith|21|23|1|2|cell-valign|c>|<cwith|23|23|1|2|cell-valign|c>|<cwith|19|19|1|2|cell-valign|c>|<cwith|17|17|1|2|cell-valign|c>|<cwith|17|17|1|2|cell-valign|c>|<cwith|17|17|1|2|cell-valign|c>|<twith|table-hyphen|y>|<table|<row|<cell|<text|Term>>|<cell|<text|Condition>>>|<row|<cell|>|<cell|>>|<row|<cell|<tabular*|<tformat|<twith|table-valign|T>|<cwith|1|-1|1|-1|cell-halign|L>|<cwith|1|-1|1|-1|cell-lsep|0ln>|<cwith|1|-1|1|-1|cell-rsep|0ln>|<table|<row|<cell|v<rsub|\<ast\>>
      v v v>>|<row|<cell|v v<rsub|\<ast\>> v v>>|<row|<cell|v v
      v<rsub|\<ast\>> v>>|<row|<cell|v v v
      v<rsub|\<ast\>>>>>>>>|<cell|<mf|s>\<gtr\><frac|d-\<sigma\>|2>+<around*|\||O<around*|(|<wide|\<varepsilon\>|~>|)>|\|>>>|<row|<cell|>|<cell|>>|<row|<cell|<tabular*|<tformat|<twith|table-valign|T>|<cwith|1|-1|1|-1|cell-halign|L>|<cwith|1|-1|1|-1|cell-lsep|0ln>|<cwith|1|-1|1|-1|cell-rsep|0ln>|<table|<row|<cell|v<rsub|\<ast\>>
      \ v F v>>|<row|<cell|v v<rsub|\<ast\>> F v>>|<row|<cell|v<rsub|\<ast\>>
      v v F>>|<row|<cell|v v<rsub|\<ast\>> v F>>|<row|<cell|v v
      v<rsub|\<ast\>>F>>|<row|<cell|v v F
      v<rsub|\<ast\>>>>>>>>|<cell|S\<geq\><around*|\||O<around*|(|<wide|\<varepsilon\>|~>|)>|\|>>>|<row|<cell|>|<cell|>>|<row|<cell|<tabular*|<tformat|<twith|table-valign|T>|<cwith|1|-1|1|-1|cell-halign|L>|<cwith|1|-1|1|-1|cell-lsep|0ln>|<cwith|1|-1|1|-1|cell-rsep|0ln>|<table|<row|<cell|v<rsub|\<ast\>>
      \ v F F>>|<row|<cell|v v<rsub|\<ast\>> F
      F>>>>>>|<cell|S\<geq\><around*|\||O<around*|(|<wide|\<varepsilon\>|~>|)>|\|>>>|<row|<cell|>|<cell|>>|<row|<cell|<tabular*|<tformat|<twith|table-valign|T>|<cwith|1|-1|1|-1|cell-halign|L>|<cwith|1|-1|1|-1|cell-lsep|0ln>|<cwith|1|-1|1|-1|cell-rsep|0ln>|<table|<row|<cell|F
      \ v v<rsub|\<ast\>>v>>|<row|<cell|v F
      \ \ v<rsub|\<ast\>>v>>|<row|<cell|F \ v v
      v<rsub|\<ast\>>>>|<row|<cell|v F v v<rsub|\<ast\>>>>>>>>|<cell|S\<geq\><around*|\||O<around*|(|<wide|\<varepsilon\>|~>|)>|\|>>>|<row|<cell|>|<cell|>>|<row|<cell|F
      \ F v v<rsub|\<ast\>>>|<cell|S\<gtr\><around*|\||O<around*|(|<wide|\<varepsilon\>|~>|)>|\|>>>|<row|<cell|>|<cell|>>|<row|<cell|<tabular*|<tformat|<twith|table-valign|T>|<cwith|1|-1|1|-1|cell-halign|L>|<cwith|1|-1|1|-1|cell-lsep|0ln>|<cwith|1|-1|1|-1|cell-rsep|0ln>|<table|<row|<cell|v
      \ F F v<rsub|\<ast\>>>>|<row|<cell|F v F
      v<rsub|\<ast\>>>>>>>>|<cell|S\<gtr\><around*|\||O<around*|(|<wide|\<varepsilon\>|~>|)>|\|>>>|<row|<cell|>|<cell|>>|<row|<cell|<tabular*|<tformat|<twith|table-valign|T>|<cwith|1|-1|1|-1|cell-halign|L>|<cwith|1|-1|1|-1|cell-lsep|0ln>|<cwith|1|-1|1|-1|cell-rsep|0ln>|<table|<row|<cell|v
      \ F \ v<rsub|\<ast\>>F>>|<row|<cell|F v
      \ v<rsub|\<ast\>>F>>>>>>|<cell|S\<gtr\><around*|\||O<around*|(|<wide|\<varepsilon\>|~>|)>|\|>>>|<row|<cell|>|<cell|>>|<row|<cell|<tabular*|<tformat|<twith|table-valign|T>|<cwith|1|-1|1|-1|cell-halign|L>|<cwith|1|-1|1|-1|cell-lsep|0ln>|<cwith|1|-1|1|-1|cell-rsep|0ln>|<table|<row|<cell|v<rsub|\<ast\>>
      F v v>>|<row|<cell|F v<rsub|\<ast\>>v
      v>>>>>>|<cell|<choice|<tformat|<table|<row|<cell|S\<gtr\><around*|\||O<around*|(|<wide|\<varepsilon\>|~>|)>|\|>>>|<row|<cell|S\<gtr\><around*|(|<frac|d-\<sigma\>|d-1>|)><around*|(|<frac|<around*|(|d-1|)>|2>-<around*|(|\<sigma\>-1|)>|)>+<around*|\||O<around*|(|<wide|\<varepsilon\>|~>|)>|\|>>>>>>>>|<row|<cell|>|<cell|>>|<row|<cell|F
      \ F v<rsub|\<ast\>>v>|<cell|S\<gtr\><frac|<around*|(|d-\<sigma\>|)><rsup|2>|4<around*|(|d-1|)>>+<around*|\||O<around*|(|<wide|\<varepsilon\>|~>|)>|\|>>>|<row|<cell|>|<cell|>>|<row|<cell|<tabular*|<tformat|<twith|table-valign|T>|<cwith|1|-1|1|-1|cell-halign|L>|<cwith|1|-1|1|-1|cell-lsep|0ln>|<cwith|1|-1|1|-1|cell-rsep|0ln>|<table|<row|<cell|F
      \ F F v<rsub|\<ast\>>>>>>>>|<cell|S\<gtr\><frac|d-\<sigma\>|6>+<around*|\||O<around*|(|<wide|\<varepsilon\>|~>|)>|\|>>>|<row|<cell|>|<cell|>>|<row|<cell|F
      \ F v<rsub|\<ast\>> F>|<cell|<choice|<tformat|<table|<row|<cell|S\<gtr\><frac|d-\<sigma\>|6>+<around*|\||O<around*|(|<wide|\<varepsilon\>|~>|)>|\|>>>|<row|<cell|S\<gtr\><frac|1|2><around*|(|<frac|d-1|2>-<around*|(|\<sigma\>-1|)>|)>+<around*|\||O<around*|(|<wide|\<varepsilon\>|~>|)>|\|>>>>>>>>|<row|<cell|>|<cell|>>|<row|<cell|<tabular*|<tformat|<twith|table-valign|T>|<cwith|1|-1|1|-1|cell-halign|L>|<cwith|1|-1|1|-1|cell-lsep|0ln>|<cwith|1|-1|1|-1|cell-rsep|0ln>|<table|<row|<cell|v<rsub|\<ast\>>
      \ F \ F F>>|<row|<cell|F v<rsub|\<ast\>> \ F
      F>>>>>>|<cell|<choice|<tformat|<table|<row|<cell|S\<geq\><frac|d-\<sigma\>|6>+<around*|\||O<around*|(|<wide|\<varepsilon\>|~>|)>|\|>>>|<row|<cell|S\<geq\><frac|d-\<sigma\>|2>-<around*|(|\<sigma\>-1|)>+<around*|\||O<around*|(|<wide|\<varepsilon\>|~>|)>|\|>>>>>>>>|<row|<cell|>|<cell|>>|<row|<cell|<tabular*|<tformat|<twith|table-valign|T>|<cwith|1|-1|1|-1|cell-halign|L>|<cwith|1|-1|1|-1|cell-lsep|0ln>|<cwith|1|-1|1|-1|cell-rsep|0ln>|<table|<row|<cell|v<rsub|\<ast\>>
      \ F F v>>|<row|<cell|F v<rsub|\<ast\>>F
      v>>>>>>|<cell|<choice|<tformat|<table|<row|<cell|S\<geq\><frac|<around*|(|d-\<sigma\>|)><rsup|2>|4<around*|(|d-1|)>>+<around*|\||O<around*|(|<wide|\<varepsilon\>|~>|)>|\|>>>|<row|<cell|S\<geq\><frac|<around*|(|d-\<sigma\>|)>|2><frac|<around*|(|d+1-2\<sigma\>|)>|<around*|(|d+\<sigma\>-2|)>>+<around*|\||O<around*|(|<wide|\<varepsilon\>|~>|)>|\|>>>|<row|<cell|S\<geq\><frac|d-\<sigma\>|2>-<around*|(|\<sigma\>-1|)>+<around*|\||O<around*|(|<wide|\<varepsilon\>|~>|)>|\|>>>>>>>>|<row|<cell|>|<cell|>>|<row|<cell|<tabular*|<tformat|<twith|table-valign|T>|<cwith|1|-1|1|-1|cell-halign|L>|<cwith|1|-1|1|-1|cell-lsep|0ln>|<cwith|1|-1|1|-1|cell-rsep|0ln>|<table|<row|<cell|v<rsub|\<ast\>>
      \ F v F \ >>|<row|<cell|F v<rsub|\<ast\>> v F
      \ >>>>>>|<cell|<choice|<tformat|<table|<row|<cell|S\<geq\><frac|<around*|(|d-\<sigma\>|)><rsup|2>|2<around*|(|d-1|)>>-<frac|<around*|(|\<sigma\>-1|)>|2>+<around*|\||O<around*|(|<wide|\<varepsilon\>|~>|)>|\|>>|<cell|>|<cell|S\<geq\><frac|\<sigma\>-1|2>,>>|<row|<cell|S\<geq\><frac|<around*|(|d-\<sigma\>|)>|2><frac|<around*|(|d+1-2\<sigma\>|)>|<around*|(|d+\<sigma\>-2|)>>+<around*|\||O<around*|(|<wide|\<varepsilon\>|~>|)>|\|>>|<cell|<text|if>>|<cell|<frac|\<sigma\>-1|2>-<frac|d-\<sigma\>|2>\<leq\>S\<less\><frac|\<sigma\>-1|2>.>>|<row|<cell|S\<geq\><around*|\||O<around*|(|<wide|\<varepsilon\>|~>|)>|\|>>|<cell|>|<cell|S\<leq\><frac|\<sigma\>-1|2>-<frac|d-\<sigma\>|2>>>>>>>>>>>
    </equation*>

    Putting all together we get

    <\equation*>
      S\<gtr\>S<rsub|min><value|eqd><around*|(|d-\<sigma\>|)><choice|<tformat|<table|<row|<cell|<frac|1|6>>|<cell|>|<cell|<frac|d+2|3>\<less\>\<sigma\>>>|<row|<cell|<frac|<around*|(|d-\<sigma\>|)>|4<around*|(|d-1|)>>>|<cell|>|<cell|<frac|d+1+<sqrt|3>|2+<sqrt|3>>\<less\>\<sigma\>\<less\><frac|d+2|3>>>|<row|<cell|<frac|1|2><frac|<around*|(|d+1-2\<sigma\>|)>|<around*|(|d+\<sigma\>-2|)>>>|<cell|>|<cell|<frac|d+4|5>\<less\>\<sigma\>\<less\><frac|d+1+<sqrt|3>|2+<sqrt|3>>>>|<row|<cell|<frac|<around*|(|d-1|)>-3<around*|(|\<sigma\>-1|)>|2<around*|(|d-\<sigma\>|)>>>|<cell|>|<cell|\<sigma\>\<less\><frac|d+4|5>>>>>>
    </equation*>

    If <math|<frac|d+2|3>\<less\>\<sigma\>>

    <\equation*>
      <tabular*|<tformat|<twith|table-valign|T>|<cwith|1|-1|1|-1|cell-halign|L>|<cwith|1|-1|1|-1|cell-lsep|0ln>|<cwith|1|-1|1|-1|cell-rsep|0ln>|<table|<row|<cell|S\<geq\><frac|d-\<sigma\>|6>>>>>>
    </equation*>

    If <math|<frac|2d+5|7>\<less\>\<sigma\>\<less\><frac|d+2|3>>

    <\equation*>
      <tabular*|<tformat|<twith|table-valign|T>|<cwith|1|-1|1|-1|cell-halign|L>|<cwith|1|-1|1|-1|cell-lsep|0ln>|<cwith|1|-1|1|-1|cell-rsep|0ln>|<table|<row|<cell|S\<geq\><frac|<around*|(|d-\<sigma\>|)><rsup|2>|4<around*|(|d-1|)>>>>>>>
    </equation*>

    If <math|<frac|d+1+<sqrt|3>|2+<sqrt|3>>\<less\>\<sigma\>\<less\><frac|2d+5|7>>

    <\equation*>
      <tabular*|<tformat|<twith|table-valign|T>|<cwith|1|-1|1|-1|cell-halign|L>|<cwith|1|-1|1|-1|cell-lsep|0ln>|<cwith|1|-1|1|-1|cell-rsep|0ln>|<table|<row|<cell|<tabular*|<tformat|<twith|table-valign|T>|<cwith|1|-1|1|-1|cell-halign|L>|<cwith|1|-1|1|-1|cell-lsep|0ln>|<cwith|1|-1|1|-1|cell-rsep|0ln>|<table|<row|<cell|S\<geq\><frac|<around*|(|d-\<sigma\>|)><rsup|2>|4<around*|(|d-1|)>>>>>>>>>>>>
    </equation*>

    If <math|<frac|d+4|5>\<less\>\<sigma\>\<less\><frac|d+1+<sqrt|3>|2+<sqrt|3>>>

    <\equation*>
      <tabular*|<tformat|<twith|table-valign|T>|<cwith|1|-1|1|-1|cell-halign|L>|<cwith|1|-1|1|-1|cell-lsep|0ln>|<cwith|1|-1|1|-1|cell-rsep|0ln>|<table|<row|<cell|<tabular*|<tformat|<twith|table-valign|T>|<cwith|1|-1|1|-1|cell-halign|L>|<cwith|1|-1|1|-1|cell-lsep|0ln>|<cwith|1|-1|1|-1|cell-rsep|0ln>|<table|<row|<cell|S\<geq\><frac|<around*|(|d-\<sigma\>|)>|2><frac|<around*|(|d+1-2\<sigma\>|)>|<around*|(|d+\<sigma\>-2|)>>>>>>>>>>>>
    </equation*>

    If <math|\<sigma\>\<less\><frac|d+4|5>>

    <\equation*>
      <tabular*|<tformat|<twith|table-valign|T>|<cwith|1|-1|1|-1|cell-halign|L>|<cwith|1|-1|1|-1|cell-lsep|0ln>|<cwith|1|-1|1|-1|cell-rsep|0ln>|<table|<row|<cell|<tabular*|<tformat|<twith|table-valign|T>|<cwith|1|-1|1|-1|cell-halign|L>|<cwith|1|-1|1|-1|cell-lsep|0ln>|<cwith|1|-1|1|-1|cell-rsep|0ln>|<table|<row|<cell|S\<geq\><frac|d-\<sigma\>|2>-<around*|(|\<sigma\>-1|)>>>>>>>>>>>
    </equation*>
  </lemma>

  \;

  \;
</body>

<\initial>
  <\collection>
    <associate|info-flag|detailed>
    <associate|page-breaking|sloppy>
    <associate|page-medium|paper>
    <associate|page-screen-margin|true>
    <associate|preamble|false>
    <associate|project-flag|false>
  </collection>
</initial>

<\references>
  <\collection>
    <associate|auto-1|<tuple|1|1>>
    <associate|sec:nonlin|<tuple|1|1>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|toc>
      <vspace*|1fn><with|font-series|<quote|bold>|math-font-series|<quote|bold>|1.<space|2spc>Nonlinear
      estimates> <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-1><vspace|0.5fn>
    </associate>
  </collection>
</auxiliary>
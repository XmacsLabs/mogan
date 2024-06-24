<TeXmacs|2.1.2>

<style|<tuple|tmdoc|british>>

<\body>
  <tmdoc-title|extra symbols when <menu|Edit|Copy to|LaTeX>>

  <section|Bug Metadata>

  <\itemize>
    <item>Reporter: ustc_skq on <hlink|gitee|https://gitee.com/XmacsLabs/mogan/issues/I8HZTY>

    <item>Owner: Darcy Shen

    <item>Tester: Darcy Shen
  </itemize>

  <section|How to reproduce it>

  Select the following two lines, and then click <menu|Edit|Copy to|LaTeX>,
  and then paste in other editor, you will find there is no newline but there
  are unexpected <verbatim|<code|<code*|\n>>>.

  <\equation>
    <tabular|<tformat|<table|<row|<cell|\<phi\><around|(|<math-bf|x>|)>=<big|int><frac|d<rsup|3>*p|<around|(|2*\<pi\>|)><rsup|3>>*<frac|1|<sqrt|2*\<omega\><rsub|<math-bf|p>>>>*<around*|(|a<rsub|<math-bf|p>>+a<rsub|-<math-bf|p>><rsup|\<dagger\>>|)>*e<rsup|i<math-bf|p>\<cdot\><math-bf|x>>>>|<row|<cell|\<pi\><around|(|<math-bf|x>|)>=<big|int><frac|d<rsup|3>*p|<around|(|2*\<pi\>|)><rsup|3>>*<around|(|-i|)><sqrt|<frac|\<omega\><rsub|<math-bf|p>>|2>>*<around*|(|a<rsub|<math-bf|p>>-a<rsub|-<math-bf|p>><rsup|\<dagger\>>|)>*e<rsup|i<math-bf|p>\<cdot\><math-bf|x>>>>>>>
  </equation>

  \;

  <tmdoc-copyright|2024|Darcy Shen>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<\initial>
  <\collection>
    <associate|page-medium|papyrus>
  </collection>
</initial>
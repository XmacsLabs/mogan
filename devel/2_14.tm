<TeXmacs|2.1.2>

<style|<tuple|tmdoc|chinese|devel>>

<\body>
  <tmdoc-title|Use lua to generate s7 glue>

  <section|Feature Metadata>

  <\description>
    <item*|Owner>jingkaimori
  </description>

  <section|Description>

  <subsection|What>

  A lua module is developed to generate scheme bindings from cpp code, which
  is described by lua modules named as <verbatim|glue_xxx.lua>. These scheme
  bindings is called \Pglue\Q in documents of texmacs.

  <subsection|Why>

  Standalone S7 program is hard to use. For example, it is hard in S7 to
  replace a pattern to another string, and migrating existing glue generator
  in Guile scheme to S7 is also cumbersome.\ 

  A lua runtime is provided by xmake, which is easier to use than S7 scheme.

  <subsection|How>

  <tmdoc-copyright|2023|jingkaimori>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<\initial>
  <\collection>
    <associate|page-medium|paper>
  </collection>
</initial>
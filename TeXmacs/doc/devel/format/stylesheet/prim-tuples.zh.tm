<TeXmacs|2.1.3>

<style|<tuple|tmdoc|chinese|old-spacing|old-dots|old-lengths>>

<\body>
  <tmdoc-title|元组操作>

  <\explain>
    <explain-macro|tuple|expr-1|<math|\<cdots\>>|expr-n><explain-synopsis|元组的构造>
  <|explain>
    构造一个元组.
  </explain>

  <\explain>
    <explain-macro|is-tuple|expr><explain-synopsis|元组测试>
  <|explain>
    测试表达式<src-arg|expr>是否返回一个元组.
  </explain>

  <\explain>
    <explain-macro|length|expr><explain-synopsis|元组的长度>
  <|explain>
    如果<src-arg|expr>是一个元组，则返回其参数数量.例如<inactive*|<length|<tuple|hop|hola>>>得到<length|<tuple|hop|hola>>.
  </explain>

  <\explain>
    <explain-macro|look-up|tuple|which><explain-synopsis|访问元组的元素>
  <|explain>
    返回元组<src-arg|tuple>的第<src-arg|which>个元素.例如，<inactive*|<look-up|<tuple|a|b|c>|1>>得到<look-up|<tuple|a|b|c>|1>.
  </explain>

  <\explain>
    <explain-macro|range|expr|start|end><explain-synopsis|抽取出子元组>
  <|explain>
    返回元组<src-arg|expr>从<src-arg|start>到<src-arg|end>（不包含）的子元组.例如，<inactive*|<range|<tuple|a|hola|hop|b|c>|2|4>>得到<range|<tuple|a|hola|hop|b|c>|2|4>.
  </explain>

  <\explain>
    <explain-macro|merge|expr-1|<math|\<cdots\>>|expr-n><explain-synopsis|元组的合并>
  <|explain>
    用于合并元组.例如，<inactive*|<merge|<tuple|1|2>|<tuple|3|4|5>>>产生<merge|<tuple|1|2>|<tuple|3|4|5>>.
  </explain>

  <tmdoc-copyright|2004|Joris van der Hoeven>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>
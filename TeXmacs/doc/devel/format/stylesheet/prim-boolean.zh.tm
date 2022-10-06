<TeXmacs|2.1.3>

<style|<tuple|tmdoc|old-spacing|old-dots|old-lengths>>

<\body>
  <tmdoc-title|布尔运算>

  <\explain>
    <explain-macro|or|expr-1|<math|\<cdots\>>|expr-n>

    <explain-macro|and|expr-1|<math|\<cdots\>>|expr-n>
  <|explain>
    返回多个表达式的或和与.例如，<inactive*|<or|false|<equal|1|1>|false>>
    = <or|false|<equal|1|1>|false>.
  </explain>

  <\explain>
    <explain-macro|xor|expr-1|expr-2>
  <|explain>
    返回两个表达式的异或, <abbr|i.e.><inactive*|<xor|true|true>>得到<xor|true|true>.
  </explain>

  <\explain>
    <explain-macro|not|expr>
  <|explain>
    返回表达式<src-arg|expr>的否定.
  </explain>

  <tmdoc-copyright|2004|Joris van der Hoeven>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>
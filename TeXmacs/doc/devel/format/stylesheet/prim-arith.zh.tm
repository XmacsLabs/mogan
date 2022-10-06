<TeXmacs|2.1.3>

<style|<tuple|tmdoc|old-spacing|old-dots|old-lengths>>

<\body>
  <tmdoc-title|算术操作>

  <\explain>
    <explain-macro|plus|expr-1|<math|\<cdots\>>|expr-n>

    <explain-macro|minus|expr-1|<math|\<cdots\>>|expr-n><explain-synopsis|加法和减法>
  <|explain>
    对数或者长度加或减.例如，<inactive*|<plus|1|2.3|5>>得到<plus|1|2.3|5>以及<inactive*|<plus|1cm|5mm>>得到<plus|1cm|5mm>.
    对于减法，其返回值是最后一个参数之前的所有参数的和减去最后一个参数所得.比如<inactive*|<minus|1>>产生<minus|1>，<inactive*|<minus|1|2|3|4>>得到<no-break><minus|1|2|3|4>.
  </explain>

  <\explain>
    <explain-macro|times|expr-1|<math|\<cdots\>>|expr-n><explain-synopsis|乘法>
  <|explain>
    返回n个表达式的乘积的值.若是长度的运算，则只允许有一个长度的表达式.比如，<inactive*|<times|3|3>>
    = <times|3|3>，<inactive*|<times|3|2cm>> = <times|3|2cm>.
  </explain>

  <\explain>
    <explain-macro|over|expr-1|<math|\<cdots\>>|expr-n><explain-synopsis|除法>
  <|explain>
    返回前n-1个表达式的积除以最后一个表达式所得的值.例如，<inactive*|<over|1|2|3|4|5|6|7>>
    = <over|1|2|3|4|5|6|7>，<inactive*|<over|3spc|7>> =
    <over|3spc|7>，<inactive*|<over|1cm|1pt>> = <over|1cm|1pt>.
  </explain>

  <\explain>
    <explain-macro|div|expr-1|expr-2>

    <explain-macro|mod|expr-1|expr-2><explain-synopsis|除法和模>
  <|explain>
    例如<inactive*|<div|18|7>>=<div|18|7> and
    <inactive*|<mod|18|7>>=<mod|18|7>.
  </explain>

  <\explain>
    <explain-macro|equal|expr-1|expr-2>

    <explain-macro|unequal|expr-1|expr-2>

    <explain-macro|less|expr-1|expr-2>

    <explain-macro|lesseq|expr-1|expr-2>

    <explain-macro|greater|expr-1|expr-2>

    <explain-macro|greatereq|expr-1|expr-2><explain-synopsis|数或长度比较>
  <|explain>
    返回值是布尔类型的.例如，<inactive*|<less|123|45>>返回<less|123|45>，<inactive*|<less|123mm|45cm>>返回<less|123mm|45cm>.
  </explain>

  <tmdoc-copyright|2004|Joris van der Hoeven>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>
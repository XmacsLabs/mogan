<TeXmacs|2.1.3>

<style|<tuple|tmdoc|chinese|old-spacing|old-dots|old-lengths>>

<\body>
  <tmdoc-title|输入大型分隔符>

  数学公式中的括号必须匹配：在你输入\P(\Q时，<TeXmacs>会自动插入\P)\Q。你可以<menu|Edit|Preferences|Keyboard|Automatic
  brackets|Disable>禁用此特性。注意:
  文档中旧有的括号可能会自动与你输入的括号匹配。

  有时，你不需要封闭的括号，或者你的括号匹配的种类不同。没有关系：如果光标正好在右括号的前面<math|<around*|(|a,b<cursor>|)>>，此时按下<key|]>可以得到表达式<math|<around|(|a,b|]><cursor>>。类似地，删除一个括号会导致其变为<hlink|不可见括号|../semantics/man-semantics-symbols.en.tm#nobracket>，然后你就可以使用任意类型左或者右括号去替换。<math|>

  默认地，括号的大小会随着其中公式的大小自动调整。使用<prefix|math:small>键前缀产生的小分隔符大小不会随之改变。你还可以使用<shortcut|(alternate-toggle
  (focus-tree))>让分隔符在大小之间切换。

  对某些分隔符，如 <math|\|>，其左右符号恰好一致。比如输入垂直方向上的竖条<key|\|>产生的是绝对的值。小竖条分隔符通过<shortcut|\|>得到，也可以是<key|\|
  var>。大竖条分隔符通过<shortcut|(math-separator "\|"
  #t)>。在<TeX>和<LaTeX>中，这类大型分隔符是不存在的；他们可以用来输入下列公式

  <\equation*>
    <around|\<langle\>|<frac|a|b+c><mid|\|><frac|p|q+r><mid|\|><frac|a|b+c>|\<rangle\>>.
  </equation*>

  中间分隔符也是非常常见的一种分隔符。比如，二元关系整除可以通过<shortcut|\<divides\>>或
  <key|\| var var var var>得到。

  在<TeXmacs>中，大型分隔符分为左分隔符，右分隔符和中间分隔符。默认地，(，[，{和<math|<around*|\<langle\>||\<nobracket\>>>是左分隔符；
  <math|)>，<math|]>，<math|}>和<math|\<rangle\>>是右分隔符。使用<prefix|math:left>，
  <prefix|math:right>和<prefix|math:middle>可以改变分隔符的状态。比如，<key|math:left
  )>产生的<math|)>，可以认为是大型左分隔符。

  有时候，你可能需要固定大小的大型分隔符，而不是自动调整。这可以通过改变分隔符之间的表达式大小的方法实现，具体是使用菜单项<menu|Format|Transform|Resize
  object>。

  还可以使用<shortcut|(math-bracket-open "." "."
  #t)>输入一对不可见的括号。这对于计算性质的文本非常有用，特别是在公式不仅仅是显示的语义，还要照顾到其精度。还可以用<shortcut|(make-rigid)>将公式保护在固定框内，即防止公式被连字符强行断开。

  <tmdoc-copyright|1998\U2020|Joris van der Hoeven|沈达>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>
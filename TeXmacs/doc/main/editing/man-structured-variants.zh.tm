<TeXmacs|2.1.3>

<style|<tuple|tmdoc|chinese|old-spacing|old-dots|old-lengths>>

<\body>
  <tmdoc-title|结构化变元>

  在创建定理，公式或者列表这些环境的时候，我们经常需要在事后更改环境。您可以使用<shortcut|(variant-circulate
  (focus-tree) #t)>或者<shortcut|(variant-circulate (focus-tree)
  #f)>循环选择最深处标记的<em|结构化变元>。前者顺序，后者逆序。

  假定您的光标在定理中，那么不断按下<shortcut|(variant-circulate
  (focus-tree) #t)>会在命题，引理，推论，证明，定理间轮换。而不断按下快捷键<shortcut|(variant-circulate
  (focus-tree) #f)>则是逆向轮换:定理 <math|\<rightarrow\>> 证明
  <math|\<rightarrow\>> 推论 <math|\<rightarrow\>> 引理
  <math|\<rightarrow\>> 命题 <math|\<rightarrow\>> 证明。

  在数学公式这个例子中， <shortcut|(variant-circulate (focus-tree)
  #t)>用于将行内公式 <math|a<rsup|2>+b<rsup|2>=c<rsup|2>>
  转化为单行公式：

  <\equation*>
    a<rsup|2>+b<rsup|2>=c<rsup|2>
  </equation*>

  同时顾及尾随空格(trailing space)和标点符号的处理。

  <TeXmacs>还提供了<shortcut|(numbered-toggle
  (focus-tree))>快捷键以供您在编号模式和普通模式间切换。此快捷键在定理，注意，表格，公式等环境适用。注意<shortcut|(numbered-toggle
  (focus-tree))>和<shortcut|(variant-circulate (focus-tree)
  #t)>的区别，前者显示和隐藏编号，而后者是在一组可用环境如列表项目(圆点，短线，箭头等)间轮换。

  <tmdoc-copyright|1998\U2020|Joris van der Hoeven|沈达>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>
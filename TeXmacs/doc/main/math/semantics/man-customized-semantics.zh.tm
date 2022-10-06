<TeXmacs|2.1.3>

<style|<tuple|tmdoc|chinese|old-spacing|old-dots|old-lengths>>

<\body>
  <tmdoc-title|定制数学语义>

  我们已经尽可能地支持了大多数经典的数学符号。然而，用户有时候需要自定义非标准语义的数学符号。而且，某些区域也可能需要一些未被支持的特殊符号。

  <TeXmacs>提供了一个非常简单的<markup|syntax>原语，以便用户手动覆盖公式的默认句法语义。假定语义编辑功能已经开启，快捷键<shortcut|(make
  'syntax)>或菜单项<menu|Insert|Semantics|Other>可插入<markup|syntax>原语。它的第一个参数是公式应该显示的模样，参数二则是公式内在的语义。

  比如，如果我们输入<math|\<cal-R\>>作为参数一，<math|\<less\>>作为参数二，那么<math|\<cal-R\>>将被解释为二元关系。更进一步，<math|\<cal-R\>>周围空格分布已经变了，可以认为是在模拟<math|\<less\>>周围的空格分布。在这个特例中，我们也可以使用<markup|math-relation>原语得到等价的结果。大部分标准操作符都可从菜单<menu|Insert|Semantics>找到，或者使用<prefix|math:syntax>键前缀。特别地，您可以使用<shortcut|(make
  'math-ignore)>简单地忽略一个公式(FIXME)，使用<shortcut|(make
  'math-ordinary)>将公式变成普通符号。

  <markup|syntax>原语与<TeXmacs>宏的结合异常灵活强大。比如，考虑这个公式<math|C=1/2*\<mathpi\>*\<mathi\>*<big|oint>f<around*|(|z|)>*\<mathd\>
  z>。<math|1/2*\<mathpi\>*\<mathi\>>很有可能应当解释为<math|1/<around*|(|2*\<mathpi\>*\<mathi\>|)>>而不是<math|<around*|(|1/2|)>*\<mathpi\>*\<mathi\>>。因此我们往往使用常量<math|2*\<mathpi\>*\<mathi\>>。首先，需要定义宏<markup|twopii>

  <\tm-fragment>
    <inactive|<assign|twopii|<inactive|<macro|<inactive|<syntax|<math|2*\<pi\>*\<mathi\>>|<math|(2*\<pi\>*\<mathi\>)>>>>>>>
  </tm-fragment>

  用户可以根据自己的偏好将这些宏聚合在一个宏包里面。未来的<TeXmacs>将提供特定区域所需符号的样式包。

  我们注意到定义公式语义的方法往往有多种。比如我们可以这样重定义<markup|twopii>这个宏:

  <\tm-fragment>
    <inactive|<assign|twopii|<inactive|<macro|<math|<around*|\<nobracket\>|2*\<mathpi\>*\<mathi\>|\<nobracket\>>>>>>>
  </tm-fragment>

  我们在<math|2*\<mathpi\>*\<mathi\>>外插入了一对不可见的括号。类似地，在公式

  <\equation*>
    \<mathe\><rsup|<sqrt|x>+\<mathe\><rsup|<sqrt|log
    x>+\<mathe\><rsup|<sqrt|log log x>+<math-ordinary|\<udots\>\<ddots\>>+log
    log log x>+log log x>+log x>,
  </equation*>

  我们可以选中整个公式，然后使用<shortcut|(make
  'math-ordinary)>赋上一个普通符号的值。然而，更优美的解决方案是，只选中子公式<math|<math-ordinary|\<udots\>\<ddots\>>>，再赋上普通符号的值。另外的例子是早先提到的符号序列<math|++-+\<nocomma\>-+>。可在不同的符号间使用<key|,
  space>快捷键插入不可见的分隔符使得这个序列可被正确的解释。

  <tmdoc-copyright|2011\U2020|Joris van der Hoeven|沈达>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>
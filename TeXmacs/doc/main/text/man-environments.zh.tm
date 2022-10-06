<TeXmacs|2.1.3>

<style|<tuple|tmdoc|chinese|old-spacing|old-dots|old-lengths>>

<\body>
  <tmdoc-title|环境>

  与属性标记类似，环境的作用是赋予一段文字以特殊含义。<hlink|属性标记|man-content-tags.zh.tm>通常作用于一小段文字，而环境的作用范围往往横跨多行。数学中常用的环境是<markup|theorem>和<markup|proof>，如下所示:

  <\theorem>
    不存在这样的正整数<math|a,b,c>和<math|n<around*|(|n\<geqslant\>3|)>>使得<math|a<rsup|n>+b<rsup|n>=c<rsup|n>>。
  </theorem>

  <\proof>
    空白太小，容不下我的证明。(费马)
  </proof>

  <menu|Insert|Environment>即插入环境。与定理类似的环境有<markup|proposition>，<compound|markup|lemma>，<markup|corollary>，<markup|axiom>，<markup|definition>。使用<markup|dueto>宏(按下<key|\\
  d u e t o return>输入该宏)可以指定与定理相关的学者，像这样:

  <\theorem>
    <dueto|毕达哥拉斯>Under nice circumstances, we have
    <math|a<rsup|2>+b<rsup|2>=c<rsup|2>>.
  </theorem>

  其它常用的环境还有<markup|remark>，<markup|note>，<markup|example>，<markup|warning>，<compound|markup|exercise>和<markup|problem>，其渲染效果与定理类似，只是没有强调显示其中内容。其余环境如<markup|verbatim>，<markup|code>，<markup|quote>，<markup|quotation>和<markup|verse>可用于输入多行文本，代码，引用或者诗歌。

  <tmdoc-copyright|1998\U2020|Joris van der Hoeven|沈达>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>
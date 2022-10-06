<TeXmacs|2.1.3>

<style|<tuple|tmdoc|chinese|old-spacing|old-dots|old-lengths>>

<\body>
  <tmdoc-title|输入数学符号>

  使用<prefix|math:greek>输入希腊字母，比如，<key|math:greek
  a>输入<math|\<alpha\>>，<key|math:greek
  G>输入<math|\<Gamma\>>。类似地，<prefix|math:bold>，
  <prefix|math:cal>， <prefix|math:frak> 和
  <prefix|math:bbb>可用来输入粗体，手写体，德文黑体和黑板粗体。例如，<shortcut|\<frak-m\>>
  得到<math|\<frak-m\>>， <key|S-F6 R>得到<math|\<bbb-R\>>以及<shortcut|\<b-cal-Z\>>得到。

  使用<key|var>还可以从拉丁字符得到希腊字母。比如，<key|p
  var>得到<math|\<pi\>>。<key|var>键还可以在希腊字母间变换，比如，<key|math:greek
  p var> 和<key|p var var>都可以得到<math|\<varpi\>>。输入黑板粗体的另外一种方法是输入该大写字母两次。比如，<key|Z
  Z>得到<math|\<bbb-Z\>>。

  一些符号中包含了诸多变元。比如<key|\<less\>>得到<math|\<less\>>，<key|\<less\>
  var>得到<math|\<in\>>，<key|\<less\> var var>得到<math|\<subset\>>，
  <key|\<less\> var var var>得到<math|\<prec\>>等等。反向轮换可用<key|S-var>。即，<key|\<less\>
  var var S-var>等价于 <key|\<less\> var>。

  其它数学符号可通过自然的键组合得到。比如<key|-
  \<gtr\>>得到<math|<op|\<rightarrow\>>>，<key|- -
  \<gtr\>>得到<math|<op|\<longrightarrow\>>>和<key|\<gtr\>
  =>得到<math|<op|\<geqslant\>>>。类似地，<key|\| var
  ->得到<math|<op|\<vdash\>>>， <key|\| -
  \<gtr\>>得到<math|<op|\<mapsto\>>>以及<key|- \<gtr\> \<less\>
  ->得到<math|<op|\<rightleftarrows\>>>。一般地，在数学符号的输入过程中有如下规则:

  <\description>
    <item*|<key|tab>>在变元间轮换的键。比如，<key|\<gtr\>
    =>得到<math|<op|\<geqslant\>>>，而<key|\<gtr\> =
    var>得到<math|<op|\<geq\>>>。类似地，<key|\<less\> var
    var>得到<math|<op|\<subset\>>>， <key|\<less\> var var
    =>得到<math|<op|\<subseteq\>>>以及<key|\<less\> var var =
    var>得到<math|<op|\<subseteqq\>>>。<key|P
    var>得到<math|\<wp\>>以及<key|e var>得到常量<math|\<mathe\>=exp<around|(|1|)>>。

    <item*|<key|@>>用来将符号嵌入圈或者框中。比如<key|@
    +>得到<math|<op|\<oplus\>>>以及<key|@
    x>得到<math|<op|\<otimes\>>>。类似地，<key|@ var
    +>得到<math|<op|\<boxplus\>>>。

    <item*|<key|/>>用于否定。例如，<key|=
    />得到<math|<op|\<neq\>>>以及<key|\<less\> =
    />得到<math|<neg|\<leqslant\>>>。注意<key|\<less\> = var var
    />得到<math|<op|\<nleqq\>>>，而 <key|\<less\> = var var /
    var>得到<math|<op|\<lneqq\>>>。

    <item*|<key|!>>用于强制使得箭头的上标或下标置于箭头的竖直方向上。比如，<key|-
    - \<gtr\> ^ x>得到<math|<op|\<longrightarrow\><rsup|x> >>，而<key|- -
    \<gtr\> ! ^ x>得到<math|\<longrightarrowlim\><rsup|x>>。
  </description>

  逻辑关系<math|\<wedge\>>和<math|\<vee\>>通过<key|&>和<key|%>得到。操作符<math|\<cap\>>和<math|\<cup\>>是其自然而然的变元，可用<key|&
  var>和<key|% var>得到。许多杂七杂八的符号可通过<prefix|math:symbol>得到。

  注意到某些符号的数学意义不一样，然而长得很是相似;这类符号被称为homoglyphs。比如竖直条\|可以是集合定义中的分隔符，如<math|R<rsup|\<gtr\>>=<around*|{|x\<in\>R\|x\<gtr\>0|}>>，也可以是二元关系整除，如<math|11\<divides\>1001>。往往，homoglyphs之间的空白是不同。最恼人的多意性可以以空白为例，如不可见的乘法空白
  <math|x*y> 和函数空白 <math|sin x>，两者分别是使用<key|*>
  和<key|space>输入的。

  为了方便<TeXmacs>自动处理你的文档以方便数学公式的语法检查等，我们鼓励用户在输入公式时注意到homoglyph的问题。更多信息，可参见<hlink|数学符号的语义|../semantics/man-semantics-symbols.zh.tm>。

  <tmdoc-copyright|1998\U2020|Joris van der Hoeven|沈达>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>
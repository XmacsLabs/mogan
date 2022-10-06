<TeXmacs|2.1.3>

<style|<tuple|tmdoc|old-spacing|chinese|old-dots|old-lengths>>

<\body>
  <tmdoc-title|数学符号的语义>

  <TeXmacs>中的数学符号都有一些属性，这些属性对应于他们想要表示的含义。例如，
  <TeXmacs>能够知道<math|<op|+>>是一个中缀运算符，!是一个后缀，以及分隔符。

  重要的数学常量<math|\<mathe\>=2.71828\<cdots\>>,
  <math|\<mathpi\>=3.14159\<cdots\>>和<math|\<mathi\>>在<TeXmacs>中是用特殊的符号表示的。这些符号和纯粹的字符<math|e>,
  <math|\<pi\>>和<math|i>有着不同的字形，可以通过快捷键<shortcut|\<mathe\>>,
  <shortcut|\<mathpi\>>和<shortcut|\<mathi\>>来输入。我们建议系统性地使用这些快捷键。

  相反，语义上不同的符号可能在字形上很相像。比如<math|f<around|(|x,y|)>>中的逗号分隔符，与<math|3\<comma\>14159\<cdots\>>中的小数逗号意义完全不同。请注意，这两个符号的间距规则是不一样的。语义不同但字形相同的符号称作<em|同形字符>。
  注意我们的语义是纯语法的：例如，<math|+>中缀通常用于加法，但有时也用来连接字符串。然而，这两种用法在语法上没有区别，因为<math|+>符号仍然是一个二元中缀，在与其他符号的优先级上，两者也是一致的。

  最容易混淆的同形字符是<TeXmacs>支持的非可见字符:

  <\itemize>
    <item>乘积，输入<key|*>。例如：<math|a*b>。

    <item>函数，输入<key|space>。例如：<math|sin x>。

    <item>非可见分隔符，输入<shortcut|\<nocomma\>>。例如：矩阵<math|A=<around|(|a<rsub|i\<nocomma\>j>|)>>。

    <item>非可见加法，输入<shortcut|\<noplus\>>。例如：<math|17\<noplus\><frac*|3|8>>。

    <item>非可见符号，输入<shortcut|\<nosymbol\>>。例如：递增
    <math|\<nosymbol\>+1>。

    <item><label|nobracket>非可见括号（主要用于内部情况）。一组匹配的非可见括号用<key|(
    var>输入。
  </itemize>

  再一次建议大家输入这些非可见符号的时候格外小心。尤其是区分乘法和函数的时候要注意，因为没有100%准确的方法来区分这二者（我们前面已经提到了<math|a<around|(|b+c|)>>和<math|f<around|(|x+y|)>>）。

  <TeXmacs>支持两种非常通用的输入同形字符的方式。一方面，我们依赖于标准变体系统。例如，<math|\<times\>>和<math|\<ast\>>分别通过<shortcut|\<times\>>和<shortcut|\<ast\>>来得到。在表<nbsp><reference|homoglyph-table>中我们给出了<TeXmacs>支持的同形字符的完整列表。

  <big-table|<block|<tformat|<table|<row|<cell|快捷键>|<cell|字形>|<cell|例子>|<cell|语义>>|<row|<cell|<key|*>>|<cell|>|<cell|<math|a*b>>|<cell|乘法>>|<row|<cell|<key|space>>|<cell|>|<cell|<math|sin
  x>>|<cell|函数>>|<row|<cell|<shortcut|\<nocomma\>>>|<cell|>|<cell|<math|a<rsub|i\<nocomma\>j>=a<rsub|j\<nocomma\>i>>>|<cell|不可见分隔符>>|<row|<cell|<shortcut|\<noplus\>>>|<cell|>|<cell|<with|mode|math|17\<noplus\><frac*|3|8>>>|<cell|不可见加法>>|<row|<cell|<shortcut|\<nosymbol\>>>|<cell|>|<cell|<math|\<nosymbol\>+1>>|<cell|不可见符号>>|<row|<cell|<key|(
  var>>|<cell|>|<cell|<math|\<Phi\>\<equiv\><around*|\<nobracket\>|\<forall\>x,P<around*|(|x|)>|\<nobracket\>>>>|<cell|不可见括号>>|<row|<cell|<key|\|>>|<cell|<math|\|>>|<cell|<math|<around*|\||-x|\|>=<around*|\||x|\|>>>|<cell|绝对值>>|<row|<cell|<key|\|
  var>>|<cell|<math|\|>>|<cell|<math|<around*|{|x\<in\>\<bbb-R\>\|x\<gtr\>0|}>>>|<cell|分隔竖线>>|<row|<cell|<key|\|
  var var>>|<cell|<math|\|>>|<cell|<math|<around*|\<langle\>|a<rsub|i><rsup|2><mid|\|>a<rsub|j><rsup|2>|\<rangle\>>>>|<cell|可拉伸的中间竖线>>|<row|<cell|<key|\|
  var var var var>>|<cell|<math|\|>>|<cell|<math|11\<divides\>1001>>|<cell|整除关系>>|<row|<cell|<key|,>>|<cell|,>|<cell|<math|f<around*|(|x,y|)>>>|<cell|逗号分隔符>>|<row|<cell|<key|,
  var>>|<cell|,>|<cell|<math|123\<comma\>456>>|<cell|十进制数字逗号>>|<row|<cell|<key|.>>|<cell|.>|<cell|<math|123.456>>|<cell|十进制数字小数点>>|<row|<cell|<key|.
  var>>|<cell|.>|<cell|<math|\<mathlambda\>x\<point\>x<rsup|2>>>|<cell|点号连接符>>|<row|<cell|<key|*
  var var var>>|<cell|<math|\<cdot\>>>|<cell|<math|\<b-v\>\<cdot\>\<b-w\>>>|<cell|点乘>>|<row|<cell|<key|.
  var var>>|<cell|<math|\<cdummy\>>>|<cell|<math|\<cdummy\>+1>>|<cell|Dummy通配符>>|<row|<cell|<key|:>>|<cell|<math|:>>|<cell|<math|<around*|{|x\<in\>E:P<around*|(|x|)>|}>>>|<cell|分隔符>>|<row|<cell|<shortcut|\<of\>>>|<cell|<math|\<of\>>>|<cell|<math|x\<of\><math-ss|Int>>>|<cell|类型满足>>|<row|<cell|<shortcut|\<over\>>>|<cell|<math|\<over\>>>|<cell|<math|121\<over\>11=11>>|<cell|除法>>|<row|<cell|<key|\\
  var>>|<cell|<math|\\>>|<cell|<math|\\x>>|<cell|反斜杠>>|<row|<cell|<key|\\
  var var>>|<cell|<math|\\>>|<cell|<math|\<bbb-N\><rsup|\<gtr\>>=\<bbb-N\>\<setminus\><around*|{|0|}>>>|<cell|集合减法>>|<row|<cell|<key|&>>|<cell|<math|\<wedge\>>>|<cell|<math|1=1\<wedge\>2=2>>|<cell|逻辑与>>|<row|<cell|<key|*
  &>>|<cell|<math|\<exterior\>>>|<cell|<math|\<mathd\>x\<exterior\>\<mathd\>y>>|<cell|楔积>>>>>|<label|homoglyph-table><TeXmacs>支持的同形异义字符.>

  <tmdoc-copyright|2018|Joris van der Hoeven|Alexander Misel>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>
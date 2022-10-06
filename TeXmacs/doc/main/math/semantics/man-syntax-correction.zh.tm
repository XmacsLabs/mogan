<TeXmacs|2.1.3>

<style|<tuple|tmdoc|old-spacing|chinese|old-dots|old-lengths>>

<\body>
  <tmdoc-title|常见错误与语法纠正>

  默认情况下，语义编辑模式\P能够理解\Q最经典的数学符号。这是通过使用专门为主流数学设计的语法实现的。显然，使用固定的语法会导致下列问题：\ 

  <\itemize>
    <item>数学公式常常会包含一些<em|特别的>记号。例如，公式可能会含有一些文字或有含义的空格。另一个<with|font-shape|italic|特别的>记号的例子是符号序列<math|++-+-+>。这些情况下，用户应该<hlink|明确注明|semantics/man-semantic-annotation.en.tm>公式的合适部分，这样这些公式才会在语义上有意义。

    <item>用来解释数学公式的<TeXmacs>语法可能在某些情况下不够完备。可以使用<TeXmacs>宏机制来自定义或扩展该语法。特定领域的符号可以写在一个专用的样式包里。
  </itemize>

  除了这些本质上难以避免的问题，下面是一些常见而且\P容易犯\Q的错误，这些错误会导致语义关联到数学公式出现问题：

  <\itemize>
    <item>由于<TeXmacs>是一个所见即所得的编辑器，一些文档结构对用户来说是不可见的。比如，数学公式<math|x+y>的呈现意味着用到了斜体和特殊的空格。而对于公式<math|f<around*|(|x|)>>，很容易把右半个括号输入到公式外面，但看起来没什么不同。

    <item>许多数学符号是有歧义的。比如，<math|a*<around*|(|b+c|)>>通常会理解为<math|a\<cdot\><around*|(|b+c|)>>，而<math|f<around*|(|x+y|)>>则会理解为一个函数应用。在语义编辑模式，用户要消除这个歧义就需要手动用<key|*>加入乘号和<key|space>加入空格。乘法/函数应用歧义是语法错误的主要来源之一，因为许多用户不会注意不可见的区别。同样，
    <math|\<wedge\>>字形既可以表示\P逻辑与\Q也可以表示\P楔积\Q。这个\P同形异义\Q问题在<hlink|数学符号的语义|semantics/man-semantics-symbols.zh.tm>一节给出了更详细的解释。\ 

    <item>有可能一段文字最初是用<LaTeX>或者老版本的<TeXmacs>写的。在这种情况下，文档不会包含大运算符范围内的括号匹配的信息。例如，在公式<math|<around*|[|x,y|[>>中，我们是否应该把第二个括号看成是右半个括号呢？而这是右侧开区间的法国标准记法。更一般地，在把语义与已有文档关联起来的时候，有可能会出现我们前面提到的各种问题。
  </itemize>

  在开启语义编辑模式之后，要检查一个公式是否是正确的，就要把光标移到公式内部，然后观察<hlink|语义焦点|semantics/man-semantics.en.tm#semantic-focus>的外框的颜色：绿色代表公式正确，红色代表存在错误。或者，如果你的焦点在一个数学公式上的话，可以点<menu|Focus|Preferences|Highlight
  incorrect formulas>，这样所有不正确的公式都会高亮，用红色方框标出。

  对于第二种\P容易犯的\Q错误，
  <TeXmacs>提供了一个自动语法修正器。把光标焦点放在公式上之后，就可以用<menu|Edit|Correct|Correct
  all>来修正你文档中所有的公式，或只修正选中部分的公式。
  如果启用了版本工具，可以用<menu|Edit|Correct|Correct
  manually>来查看原文档和修正过的文档的差异。你可以用版本工具来对比这些差异，并选择合适的版本。

  具体用哪种算法来修正可以通过<menu|Edit|Preferences|Mathematics|Manual
  correction>来选择：

  <\description>
    <item*|删除多余的不可见运算符>该算法用于删除任何多余的函数或乘法。例如，习惯于编辑ASCII文件的用户通常在二进制中缀周围键入空格，例如加法。此算法将删除此类\P函数空格\Q。

    <item*|插入缺少的不可见运算符>在<LaTeX>中，从来不会区分乘法和函数。当导入一个<LaTeX>文档的时候，检测和加入缺少的乘法和函数空格就是十分重要的。

    <item*|同形字符替换>该算法可以对视觉上相似但语义上不同的符号做一些其它的有用替换。比如，反斜杠<math|\\>在合适的情况下会替换为二元集合差中缀运算（如在<math|X\<setminus\>Y>中）。
  </description>

  在<menu|Edit|Preferences|Mathematics|Automatic
  correction>，还可以选择打开文件时默认应用的修正算法。在导入<LaTeX>文件的时候，总是会运行一遍修正算法的。

  修正过语法之后，剩下的错误就是真正的笔误或非标准或不支持的符号了。我们也会发现，\P正确的\Q公式不一定就是表示想要的意思。要想检查是否真的表示想要的含义，就只能在输入公式的时候注意一下了。

  <tmdoc-copyright|2018|Joris van der Hoeven|Alexander Misel>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>
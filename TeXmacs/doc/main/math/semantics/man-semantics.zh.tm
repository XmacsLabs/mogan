<TeXmacs|2.1.3>

<style|<tuple|tmdoc|chinese|old-spacing|old-dots|old-lengths>>

<\body>
  <tmdoc-title| 语义编辑工具>

  自从<name|1.0.7.10>，<TeXmacs>新增了一些数学公式的语义编辑特性。合理使用这些特性，就能使您的公式至少在句法层面上保证正确。比如，在公式<math|a+b>中，计算机能识别<math|+>是作用在变量<math|a>和<math|b>上的运算符。注意这里所谓\Q语义\Q仅限于此，即<TeXmacs>不会去关注加法的数学本质。

  语义编辑需要用户注意一些额外的事物，至少是适应它们。比如，用<key|*>输入乘号和用<key|Space>输入函数是用户的需要了解且实践的知识。诚然，展现在图像上，这些操作的细节无法辨识，因为它们都是以空格的形式为用户所见。然而这些操作的语义显然截然不同。

  尽管通常情况下，就排版而言，语义正确的文档和以打印为目的的正式文档没有太多的区别。我们认为用户为此付出的努力是有价值的，具体理由如下

  <\itemize>
    <item>比如，当您输入的公式用于计算机的代数系统时，语义丰富的文档更显优势。

    <item>句法上正确的文档出现\Ptypos\Q或者更复杂的数学错误的可能性更小。

    <item>对某些编辑操作，诸如剪切和粘贴，您可以更方便的选中句法上意义完整的子公式。

    <item>这降低了您使用非标准标记的可能性，非标准标记会增加您的作品潜在用户的阅读难度。
  </itemize>

  更进一步，其它语义相关的工具其实已经集成到了某些特性中，如语义搜索和替换或者互联网上的语义搜索。

  点击<menu|Edit|Preferences|Mathematics|Semantic
  editing>激活语义编辑工具。 在语义编辑模式下，您将使用到针对公式句法结构的结构化编辑特性。比如，<em|语义焦点><label|semantic-focus>通常是<hlink|当前焦点|../../text/man-structure.en.tm>的子公式。又如，您只能选中句法上有意义的子公式。

  语义焦点的意义如下所述：首先，句法上正确的公式显示为绿色，错误的则为红色。这样您就可以在输入公式时迅速发现\Ptypos\Q。其二，如果您对某个数学操作符或者关系的优先级存在任何疑问，从语义焦点便可以看出缺省优先级。具体地，将您的光标置于操作符的右边，则该操作符所作用的子表达式会高亮显示。在加法（或者更一般的结合算符）这个例子中，所有的被加数都会高亮显示。

  <tmdoc-copyright|2013\U2020|Joris van der Hoeven|沈达>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>
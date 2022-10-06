<TeXmacs|2.1.3>

<style|<tuple|tmdoc|chinese|old-spacing|old-dots|old-lengths>>

<\body>
  <tmdoc-title|默认序列化>

  文档（对应的文件后缀是<verbatim|.tm>和<verbatim|.ts>）使用标准的<TeXmacs>语法写入到磁盘上。这种语法的设计遵循不晦涩和简单易读的原则，以使得我们在文本编辑器上就能轻易理解文档的内容。比如，上文的公式(<reference|tm-tree-ex>)表示为

  <\quote-env>
    <framed-fragment|<verbatim|\<less\>with\|mode\|math\|x+y+\<less\>frac\|1\|2\<gtr\>+\<less\>sqrt\|y+z\<gtr\>\<gtr\>>>
  </quote-env>

  在另一方面，<TeXmacs>的语法让样式文件难以阅读，其设计也不是为了手动编辑：比如空格的复杂语义和一些内部结构并不明显的表现形式。请不要在<TeXmacs>语法层面上编辑这些文档（尤其是样式文件），除非你十分清楚你在做什么。

  <paragraph*|序列化的主要原则>

  在<TeXmacs>格式中，文档树的序列化使用<verbatim|\<less\>>,
  <verbatim|\|>, <verbatim|\<gtr\>>, <verbatim|\\>和<verbatim|/>这些特殊符号。默认地，这样一棵树

  <\equation>
    <label|gen-tree-tm><tree|f|x<rsub|1>|\<cdots\>|x<rsub|n>>
  </equation>

  将被序列化为

  <\tm-fragment>
    <verbatim|\<less\>f\|x<rsub|1>\|...\|x<rsub|n>\<gtr\>>
  </tm-fragment>

  如果参数<math|x<rsub|1>,\<ldots\>,x<rsub|n>>中有一个是多个段落的文档树（在这里意思也就是其中包含着<markup|document>标签或者<markup|collection>标签）
  ，那么序列化则是另外一种长的形式。如果<verbatim|f>只包含多段落的参数，那么这棵文档树会被序列化为：

  <\tm-fragment>
    <\verbatim>
      \<less\>\\f\<gtr\>

      \ \ x<rsub|1>

      \<less\>\|f\<gtr\>

      \ \ ...

      \<less\>\|f\<gtr\>

      \ \ x<rsub|n>

      \<less\>/f\<gtr\>
    </verbatim>
  </tm-fragment>

  大体上，不包含多个段落的参数序列化时会使用短的形式。比如，如果<verbatim|n=5>而且<verbatim|x<rsub|3>>和<verbatim|x<rsub|5>>包含多个段落，<verbatim|x<rsub|1>>、<verbatim|x<rsub|2>>和<verbatim|x<rsub|4>>则不包含，那么(<reference|gen-tree-tm>)会被序列化为

  <\tm-fragment>
    <\verbatim>
      \<less\>\\f\|x<rsub|1>\|x<rsub|2>\<gtr\>

      \ \ x<rsub|3>

      \<less\>\|f\|x<rsub|4>\<gtr\>

      \ \ x<rsub|5>

      \<less\>/f\<gtr\>
    </verbatim>
  </tm-fragment>

  字符<verbatim|\<less\>>、<verbatim|\|>、<verbatim|\<gtr\>>和<verbatim|\\>分别转义为<verbatim|\\\<less\>less\\\<gtr\>>、<verbatim|\\\|>、<verbatim|\\\<less\>gtr\\\<gtr\>>和<verbatim|\\\\>。另外，<math|\<alpha\>+\<beta\>>会被序列化为<verbatim|\\\<less\>alpha\\\<gtr\>+\\\<less\>beta\\\<gtr\>>。

  <paragraph*|格式化和空格>

  <markup|document>和<markup|concat>原语以一种特殊的方式序列化。<markup|concat>原语的序列化就是普通的拼接。比如，文本\Pan
  <em|important> note\Q被序列化为

  <\tm-fragment>
    <\verbatim>
      an \<less\>em\|important\<gtr\> note
    </verbatim>
  </tm-fragment>

  <markup|document>标签的序列化是用两个换行符分隔相继的段落。比如，这段引文

  <\quote-env>
    Ik ben de blauwbilgorgel.

    Als ik niet wok of worgel,
  </quote-env>

  被序列化为

  <\tm-fragment>
    <\verbatim>
      \<less\>\\quote-env\<gtr\>

      \ \ Ik ben de blauwbilgorgel.

      \;

      \ \ Als ik niet wok of worgel,

      \<less\>/quote-env\<gtr\>
    </verbatim>
  </tm-fragment>

  注意段落两端的空格将被忽略。在段落内部多个空格将被视为一个空格。类似地，多于两个的换行符等价于两个换行符。比如，上述引文也可以被存储为

  <\tm-fragment>
    <\verbatim>
      \<less\>\\quote-env\<gtr\>

      \ \ Ik ben de \ \ \ \ \ \ \ \ \ \ blauwbilgorgel.

      \;

      \;

      \ \ Als ik niet wok of \ \ \ \ \ \ \ \ \ worgel,

      \<less\>/quote-env\<gtr\>
    </verbatim>
  </tm-fragment>

  可以用转义序列\P<verbatim|\\ >\Q明确表示空格。空段落用\P<verbatim|\\;>\Q表示。

  <paragraph*|原始数据>

  <markup|raw-data>原语用于在<TeXmacs>中表示二进制数据，比如将图像文件嵌入到文档中。这些二进制数据被序列化为

  <\tm-fragment>
    <\verbatim>
      \<less\>#<em|binary-data>\<gtr\>
    </verbatim>
  </tm-fragment>

  其中，<verbatim|<em|binary-data>>是表示一串字节的一串十六进制数字。

  <tmdoc-copyright|2016|Joris van der Hoeven>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>
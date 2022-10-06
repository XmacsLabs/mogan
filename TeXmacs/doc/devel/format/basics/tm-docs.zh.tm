<TeXmacs|2.1.3>

<style|<tuple|tmdoc|chinese|old-spacing|old-dots|old-lengths>>

<\body>
  <tmdoc-title|<TeXmacs>的文档>

  <TeXmacs>的文档片段可看作是<TeXmacs>树，而<TeXmacs>文档则是一棵特殊形式的树。下面我们就将具体描绘这棵树。<TeXmacs>文档树的根节点必须是<markup|document>标记，而其子节点必须是以下形式之一：

  <\explain|<explain-macro|TeXmacs|version><explain-synopsis|<TeXmacs>的版本>>
    这个命令标记指定了该文档的<TeXmacs>的版本。
  </explain>

  <\explain|<explain-macro|project|ref><explain-synopsis|工程>>
    文档所属的工程，可有可无。
  </explain>

  <\explain>
    <explain-macro|style|version>

    <explain-macro|style|<with|font-shape|right|<explain-macro|tuple|style|pack-1|<math|\<cdots\>>|pack-n>>><explain-synopsis|样式和宏包>
  <|explain>
    文档的样式和额外的宏包，可有可无。
  </explain>

  <\explain|<explain-macro|body|content><explain-synopsis|文档的主体>>
    这个命令标记指定了文档的主体。
  </explain>

  <\explain|<label|initial-env><explain-macro|initial|table><explain-synopsis|初始化环境>>
    文档的初始化环境，如页面大小、补白等信息，可选。<inactive|<arg|table>>的形式是<explain-macro|collection|binding-1|<math|\<cdots\>>|binding-n>。其中每一个<src-arg|binding-<no-break>i>的形式都是<explain-macro|associate|var-i|val-i>，意思是将初始值<inactive|<arg|val-i>>关联到环境变量<inactive|<arg|var-i>>上。没有出现在<inactive|<arg|table>>里面的环境变量的初始值由样式文件和宏包决定。
  </explain>

  <\explain|<explain-macro|references|table><explain-synopsis|引用>>
    文档中所有有效引用的列表，可选。其实这些信息是可以自动生成的，但需要多次扫描全文。为了使编辑器在打开文档的时候更快，就把引用直接‘‘缓存’'在文档中了。

    <inactive|<arg|table>>和<markup|initial>的类似。不同之处在于这里将元组和标签关联在了一起。元组可为<explain-macro|tuple|content|page-nr>或为<explain-macro|tuple|content|page-nr|file>。<inactive|<arg|content>>是指向标签所显示的文本，<inactive|<arg|page-nr>>是相应的页号，其中可有可无的<inactive|<arg|file>>表示标签所定义的文档（这只有在该文档属于同一工程时使用）。
  </explain>

  <\explain|<explain-macro|auxiliary|table><explain-synopsis|文档的附加数据>>
    这个标记中主要放一些文档的附加数据，可有可无。通常，这些附加数据是可以从文档正文正文中提取出来的。因为提取这些数据往往需要一定时间和额外的工具，而且你的系统上不一定有这些工具。所以存储这些数据还是非常有必要的。<inactive|<arg|table>>的格式和<markup|initial>与<markup|references>中<inactive|<arg|table>>的格式类似，将附加内容和一些键关联起来。标准的键主要有<verbatim|bib>,
    <verbatim|toc>, <verbatim|idx>, <verbatim|gly>等等。
  </explain>

  <\example>
    一篇内容为\P你好，世界！ \Q的文章表示为：

    <\equation*>
      <tree|<text|<markup|document>>|<tree|<text|<markup|TeXmacs>>|<text|<TeXmacs-version>>>|<tree|<text|<markup|style>>|article>|<tree|<text|<markup|body>>|<tree|<text|<markup|document>>|你好，世界！>>>
    </equation*>
  </example>

  <tmdoc-copyright|2004|Joris van der Hoeven>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>
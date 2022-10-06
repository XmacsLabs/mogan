<TeXmacs|2.1.3>

<style|<tuple|tmdoc|chinese|old-spacing|old-dots|old-lengths>>

<\body>
  <tmdoc-title|<name|Guile>扩展语言入门>

  与Emacs类似，<TeXmacs>使用类Lisp语言作为扩展\V\V来自GNU计划的<with|font-shape|small-caps|Guile
  Scheme>。关于<name|Guile Scheme>，请参考：

  <\verbatim>
    \ \ \ \ \ \ \ \ http://www.gnu.org/software/guile/guile.html
  </verbatim>

  <scheme>的优势在于可通过外部的C或C++程序扩展。在<TeXmacs>中，你可以使用<scheme>来创建你自己的菜单和快捷键，甚至是你自己的<TeXmacs>扩展。

  如果你已经下载了<TeXmacs>的源代码，不妨看一下这些文件：

  <\verbatim>
    \ \ \ \ \ \ \ \ Guile/Glue/build-glue-basic.scm<next-line>
    \ \ \ \ \ \ \ Guile/Glue/build-glue-editor.scm<next-line>
    \ \ \ \ \ \ \ Guile/Glue/build-glue-server.scm
  </verbatim>

  这三个胶水代码文件包含了在<name|Scheme>中可见的C++的库函数(routine)。在下面的小节中，我们将讨论其中最重要的部分。我们计划撰写更加完整的参考文档。目前你可以参考<verbatim|
  $TEXMACS_PATH/progs>目录中的<name|Scheme>源文件（以scm为后缀名）。

  <tmdoc-copyright|2013\U2019|Joris van der Hoeven|沈达>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>
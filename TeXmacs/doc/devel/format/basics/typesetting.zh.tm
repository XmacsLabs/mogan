<TeXmacs|2.1.3>

<style|<tuple|tmdoc|chinese|old-spacing|old-dots|old-lengths>>

<\body>
  <tmdoc-title|排版过程>

  为了更好的理解<TeXmacs>文档的格式，首先要了解编辑器排版文档的过程。TeXmacs文档的排版，实际上是将逻辑层面的<TeXmacs>文档树转化为视觉层面的盒子，然后显示在屏幕上或者打印在纸上的过程（请注意，实际上盒子提供的信息不仅仅用于渲染，还可以用于定位光标所在位置、确定选区等等）。

  完整的排版过程主要分为两个部分：
  使用排版原语对<TeXmacs>树求值，实际的排版过程。

  <hlink|排版原语|../regular/regular.zh.tm>设计成能被迅速渲染，且内置于编辑器。比如，有水平拼接(<markup|concat>)，分页(<markup|page-break>)，分式(<markup|frac>)，超链接(<markup|hlink>)等等。许多排版原语的渲染可以通过<hlink|内置环境变量|../environment/environment.en.tm>精确定制。比如，环境变量<src-var|color>指定了当前对象的颜色，<src-var|par-left>则指定了当前段落的左边界等等。

  <hlink|样式文件语言|../stylesheet/stylesheet.en.tm>用于在内置原语的基础上撰写新的原语（宏）。其中包含了定义宏、条件语句、逻辑和算术运算、延迟执行等等原语。样式文件语言还提供了一个特殊的标签\V\V<markup|extern>。有了这个标签，你就可以结合<scheme>撰写宏了。

  用户自定义的宏主要有两种作用。一方面，用于执行简单的重写规则。比如，这个宏

  <\tm-fragment>
    <inactive*|<assign|seq|<macro|var|from|to|<active*|<math|<inactive*|<arg|var>><rsub|<inactive*|<arg|from>>>,\<ldots\>,<inactive*|<arg|var>><rsub|<inactive*|<arg|to>>>>>>>>
  </tm-fragment>

  就是用于产生<math|a<rsub|1>,\<ldots\>,a<rsub|n>>这类序列的快捷方式。在这种重写规则中，<markup|seq>标签中的子元素<src-arg|var>，
  <src-arg|from>和<src-arg|to>是能够在编辑器中通过光标移入访问并修改的。另一方面，用户自定义的宏具有合成或计算的特征。比如，<markup|seq>标签中的三个点不能被用户更改。类似的，这个宏

  <\tm-fragment>
    <inactive*|<assign|square|<macro|x|<times|<arg|x>|<arg|x>>>>>
  </tm-fragment>

  仅用于计算。一般来说，用于合成的宏更容易编写，但是保留的可访问元素越多，该标签被用户编辑也越多。

  值得注意的是<TeXmacs>的排版会产生一些辅助数据。比如，引用数、页码以及目录和索引等都需要在排版时计算得到正确值然后存储在一个特殊位置。尽管这些辅助数据可以从文档中自动计算出来，可以这个过程也许会比较耗时（因为很有可能整个文档都需要重新排版）。尤其是在这些辅助数据的计算依赖于外部插件时，会导致在某些系统上无法重新计算。因此，在你保存文档时，辅助数据也将存储在磁盘上。

  <tmdoc-copyright|2004|Joris van der Hoeven>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>
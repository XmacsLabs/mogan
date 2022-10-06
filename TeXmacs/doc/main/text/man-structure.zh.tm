<TeXmacs|2.1.3>

<style|<tuple|tmdoc|chinese|old-spacing|old-dots|old-lengths>>

<\body>
  <tmdoc-title|输入结构化文本>

  长文档往往具有良好的结构：即章、节和子节。其中往往包含不同类型的文本，比如纯文本，引用，脚注，定理等。选择好<menu|Document|Style>后，<TeXmacs>会依照样式为文本排版，比如节、页、定理的编号，又如引用和脚注的排版等等。

  目前可用的标准文档样式有：<tmstyle|generic>，
  <tmstyle|article>， <tmstyle|book>， <tmstyle|letter>，
  <tmstyle|exam>， <tmstyle|beamer>， <tmstyle|seminar>，
  <tmstyle|source>。比如，article样式可用于写文章。另外的样式可用于期刊论文或其它特殊用途，比如<TeXmacs>官方文档的样式。

  选择好文档的样式后，您可以用节来组织文本，还可以使用特定的环境。环境的例子有定理，命题，注意等(见<menu|Insert|Enunciation>)。有序列表和无序列表也可用来组织文本。常用的属性标记有<markup|strong>，<markup|name>等等，可用于改变特定文本的属性。

  用熟<TeXmacs>后，您可在自己的样式文件中添加环境。假设您经常做引用，且您希望引用文本是斜体的，且左右边距皆为1cm。最好的方法是制作一个引用环境，而不是一次次手动的改变文本和段落的属性。这不仅仅能够加快引用的输入，还可以直接改变引用环境的定义，同步改变文档中所出现的所有引用环境。比如您需要把引用的字体全部改小，此时，直接改定义明显比一次次手动修改方便。

  了解一些通用的编辑规则可以极大的方便编辑<TeXmacs>的结构化文本。一个重要的概念是焦点，请看下面这个例子。假设我们正在输入如下经典定理：

  <\quote-env>
    下面这个定理是<name|欧拉>的：

    <\big-envbox>
      <\theorem>
        <small-focus|<math|\<mathe\><rsup|\<mathpi\>*\<mathi\>>=\<um\>1<math-cursor>>>。
      </theorem>
    </big-envbox>
  </quote-env>

  在光标处，灰色和蓝绿色的盒子显示的是激活的标记：在本例中，光标是在定理和公式中。最内部的激活被蓝绿色框包围的标记称为当前焦点。

  <menu|Focus>菜单和焦点工具栏（最后一行工具栏）是高度上下文相关的，它们都依赖于当前焦点。在本例中，焦点工具栏中有一个弹出菜单公式，当您选择<menu|Equation>后，文本会变为：

  <\quote-env>
    下面这个定理是<name|欧拉>的：

    <\big-envbox>
      <\theorem>
        \;

        <\big-focus>
          <\equation*>
            \<mathe\><rsup|\<mathpi\>*\<mathi\>>=\<um\>1<math-cursor>.
          </equation*>
        </big-focus>
      </theorem>
    </big-envbox>
  </quote-env>

  类似地，焦点工具栏左部的箭头按钮可允许您跳转到相似标记。在本例中，它们会让你迅速在文档中所有的公式或者方程间浏览。关于<hlink|结构化编辑|../editing/man-structured-editing.zh.tm>和<hlink|结构化编辑工具|../editing/man-editing-tools.zh.tm>，还请移步细看。

  第二个重要的概念是编辑模式。有五种主要的模式：文本模式，数学模式，程序模式，图形模式和源代码模式。模式切换比焦点的切换要少得多，其工具栏就在焦点工具栏的上方。<menu|Insert>和<menu|Format>菜单都是模式相关的。

  <tmdoc-copyright|1998\U2010|Joris van der Hoeven>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<\initial>
  <\collection>
    <associate|preamble|false>
  </collection>
</initial>
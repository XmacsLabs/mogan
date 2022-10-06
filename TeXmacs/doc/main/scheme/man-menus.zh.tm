<TeXmacs|2.1.3>

<style|<tuple|tmdoc|chinese|old-spacing|old-dots|old-lengths>>

<\body>
  <tmdoc-title|定制动态菜单>

  定义一个名称为<scm-arg|name>的菜单，可用

  <\scm-code>
    (menu-bind <scm-arg|name> . <scm-arg|def>)
  </scm-code>

  或者

  <\scm-code>
    (tm-menu (<scm-arg|name>) . <scm-arg|def>)
  </scm-code>

  其中 <scm-arg|def> 是一个呈现所有菜单条目的程序。下面这个目录下的文件可供参考：

  <\verbatim>
    \ \ \ \ $TEXMACS_PATH/progs/menu
  </verbatim>

  其中包含了标准<TeXmacs>菜单的定义方式。对于<scm|tm-menu>，你还可以指定额外的参数，使之根据参数动
  态构建更加复杂的菜单。

  更精确地说，在<scm|menu-bind>或者<scm|tm-menu>中的<verbatim|<em|def>>程序实际上是如下所示形式的程序代码：

  <\scm-code>
    (=\<gtr\> "pulldown menu name" <scm-arg|menu-definition>)

    (-\<gtr\> "pullright menu name" <scm-arg|menu-definition>)

    ("entry" <scm-arg|action>)

    ---

    (if <scm-arg|condition> <scm-arg|menu-definition>)

    (link <scm-arg|variable>)
  </scm-code>

  函数<scm|=\<gtr\>>和<scm|-\<gtr\>>用来创建下拉或者右拉菜单，<scm-arg|menu-definition>中要包含创建子菜单的程序。使用函数<scm|("entry"
  <scm-arg|action>)>可创建一个菜单条目，点击<scm|entry>就会执行<scm-arg|action>。菜单项之间使用<scm|--->分割。函数
  <scm|if> 用于在满足特定的 <scm|condition>
  时插入菜单项。（比如说在数学模式中）

  如果你声明了一个菜单 <scm|name> ，那么你可以使用函数
  <scm|link> 间接引用该菜单。这种间接声明子菜单的方式有两个优势：

  <\itemize>
    <item>\P间接\Q子菜单可以链接到我们所需的菜单，无论多少

    <item>使用<scm|menu-append>可以后续添加新条目到\Q间接\Q子菜单中
  </itemize>

  主要的<TeXmacs>菜单是<scm|texmacs-menu>，<scm|texmacs-popup-menu>，<scm|texmacs-main-icons>，
  <scm|texmacs-mode-icons>， <scm|texmacs-focus-icons>和<scm|texmacs-extra-icons>。其他一些标准的间接菜单是<scm|file-menu>，
  <scm|edit-menu>， <scm|insert-menu>， <scm|text-menu>，
  <scm|paragraph-menu>， <scm|document-menu>，
  <scm|options-menu>和<scm|help-menu>。

  <tmdoc-copyright|2013\U2019|Joris van der Hoeven|沈达>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>
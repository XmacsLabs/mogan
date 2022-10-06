<TeXmacs|2.1.3>

<style|<tuple|tmdoc|chinese|old-spacing|old-dots|old-lengths>>

<\body>
  <tmdoc-title|定制快捷键>

  下面这行代码用来指定键盘布局

  <\scm-code>
    (kbd-map . <scm-arg|keymaps>)
  </scm-code>

  使用<scm|:mode>选项，你可以指定使键盘布局生效的条件。比如，这行代码：

  <\scm-code>
    (kbd-map (:mode in-math?) . <scm-arg|keymaps>)
  </scm-code>

  指定了只在数学模式下生效的键盘布局。<scm-arg|keymaps>
  这个列表中的元素有下面三种形式：

  <\scm-code>
    (<em|key-combination> <scm-arg|action_1> ... <scm-arg|action_n>)

    (<em|key-combination> <scm-arg|result>)

    (<em|key-combination> <scm-arg|result> <scm-arg|help-message>)
  </scm-code>

  第一行中，<scm-arg|action_i>是和<scm-arg|key-combination>相关的Scheme代码。第二行和第三行中的
  <scm-arg|result>是<scm-arg|key-combination>结束之后插入的字符串。第三行中，<scm-arg|key-combination>
  结束之后，将会显示<scm-arg|help-message>。

  <tmdoc-copyright|2013\U2019|Joris van der Hoeven|沈达>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>
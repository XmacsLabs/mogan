<TeXmacs|2.1.3>

<style|<tuple|tmdoc|chinese|old-spacing|old-dots|old-lengths>>

<\body>
  <tmdoc-title|动态对象>

  某些更加复杂的对象在编辑的过程中可以有不同状态。这种动态对象的一个例子便是标签和引用，因为引用的标号必须动态确定。另外一些动态标记的例子可以在<hlink|撰写样式文件|../../../devel/style/keyboard/style-kbd.zh.tm>这章中找到。

  编辑进入动态对象时，比如使用<shortcut|(make-label)>插入标签，默认的状态是不激活的。在这种不激活的状态中，你可以输入和这个动态对象相关的信息，比如在标签这个例子中就是标签的名字。有一些动态对象的参数数量不是固定的，可以用<key|var>插入新的参数。

  <\big-figure>
    <with|color|blue|<math|\<langle\>>label<math|\|>>pythagoras<with|color|blue|<math|\<rangle\>>>
  </big-figure|不激活的标签>

  当你将动态对象的相关信息输入完毕后，你可以输入<shortcut|(kbd-return)>激活这个对象。在光标移动到对象后面并敲击<key|backspace>就可以反激活已经激活的动态对象。

  <tmdoc-copyright|1998\U2002|Joris van der Hoeven>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>
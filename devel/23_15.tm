<TeXmacs|2.1.2>

<style|<tuple|tmdoc|chinese|devel>>

<\body>
  <tmdoc-title|When inserting a Text At object, the alignment of
  Centre/Centre should be adopted by default.>

  This is a <strong|Feature> of <with|font-series|bold|project 23>.\ 

  <section|Feature metadata>

  <\description>
    <item*|Owner>Oyyko

    <item*|Gitee issue><href|https://gitee.com/XmacsLabs/mogan/issues/I5XGVX>

    <item*|Github issue><href|https://github.com/XmacsLabs/mogan/issues/185>
  </description>

  <section|Description>

  <subsection|Feature Description>

  Gitee Issue said that we should use <cpp|Centre/Centre> as the default
  alignment style. This can be done easily.

  Github Issue suggest that the editor should remember the alignment setting
  in the graphics tool when the user left off. This is not suitable. Because
  people may need to use different styles on different objects. Therefore,
  when the editor is trying to remember the style, it will face a problem.
  The problem is: which style should be memorized as the default style. One
  solution is to let the user customize the default style in the menu. This
  may be useful to some users, but few software can be customized so freely.
  I want to set the default style to <cpp|Centre/Centre> and temporarily
  close this Issue.

  Then, Yiqi Xu explained the current methods to change the alignment
  setting.

  <\enumerate>
    <item>When you haven't selected a specific object or unselect it by
    clicking anywhere inside the drawing area outside the object, you can
    change the alignment universally through the focus toolbar. This new
    setting will then apply to all the upcoming text/formula boxes.

    <item>When you select a text/formula box, you can individually change its
    alignment.
  </enumerate>

  He recommended starting with the implementation of remembering the setting
  changed in the universal way (1.) mentioned above.

  <subsection|Conclusion>

  After discussion, it was considered that this task could be omitted
  temporarily.

  \;

  <tmdoc-copyright|2023|Oyyko>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<\initial>
  <\collection>
    <associate|page-medium|papyrus>
  </collection>
</initial>
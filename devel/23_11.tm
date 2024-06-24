<TeXmacs|2.1.2>

<style|<tuple|tmdoc|chinese|devel>>

<\body>
  <tmdoc-title|When pasting an object, it is necessary to keep a certain
  distance from the original object.>

  This is a <strong|Bug> of <with|font-series|bold|project 23>.\ 

  <section|Bug metadata>

  <\description>
    <item*|Owner>Oyyko

    <item*|Gitee issue><href|https://gitee.com/XmacsLabs/mogan/issues/I5XGKD>

    <item*|Github issue>NA
  </description>

  <section|Description>

  <subsection|Bug recurrence>

  If pasting is done after copying an object, it will overlap. This is not
  conducive to subsequent observations. For example, in the image below there
  are actually two circles overlapping.

  <with|gr-mode|<tuple|group-edit|edit-props>|gr-frame|<tuple|scale|1cm|<tuple|0.5gw|0.5gh>>|gr-geometry|<tuple|geometry|1par|0.6par>|gr-grid|<tuple|cartesian|<point|0|0>|2>|gr-edit-grid-aspect|<tuple|<tuple|axes|none>|<tuple|1|none>|<tuple|10|none>>|gr-edit-grid|<tuple|cartesian|<point|0|0>|1>|<graphics||<carc|<point|-4.2|1.3>|<point|-3.7|1.4>|<point|-3.6|0.7>>|<carc|<point|-4.2|1.3>|<point|-3.7|1.4>|<point|-3.6|0.7>>|<carc|<point|-4.2|1.3>|<point|-3.7|1.4>|<point|-3.6|0.7>>>>

  <subsection|Solution>

  When pasting in place is detected, a small offset is performed.

  For the offset direction, the algorithm is: detect the relative position of
  the graphic in the entire canvas, if it is in the upper left corner, then
  the pasted graphic will be offset to the lower right corner. Vice versa is
  the same.

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
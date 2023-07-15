<TeXmacs|2.1.2>

<style|<tuple|tmdoc|chinese|devel>>

<\body>
  <tmdoc-title|Draw an ellipse>

  This is a <strong|Feature> of <with|font-series|bold|project 23>.\ 

  <section|Feature metadata>

  <\description>
    <item*|Owner>Oyyko

    <item*|Gitee issue><slink|https://gitee.com/XmacsLabs/mogan/issues/I6QBAE>

    <item*|Github issue>NA
  </description>

  <section|Description>

  Implement the ellipse drawing function in the drawing tool.

  Plan: refer to the code for drawing circles, and design ellipse drawing.
  For example, a circle is as below.

  <with|gr-mode|<tuple|edit|carc>|gr-frame|<tuple|scale|1cm|<tuple|0.5gw|0.5gh>>|gr-geometry|<tuple|geometry|1par|0.6par>|gr-grid|<tuple|cartesian|<point|0|0>|2>|gr-edit-grid-aspect|<tuple|<tuple|axes|none>|<tuple|1|none>|<tuple|10|none>>|gr-edit-grid|<tuple|cartesian|<point|0|0>|1>|<graphics||<carc|<point|-1.2|0.5>|<point|1.2999999999999996|1.7>|<point|0.0|-0.8>>>>

  Problems that may be encountered:\ 

  <\itemize-dot>
    <item>the current drawing toolbar is full, and it is necessary to find a
    place to store the new ellipse work. and design a new icon.

    <item>Ellipses are drawn in different ways, such as the focal point
    method or the 5-point method.
  </itemize-dot>

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
    <associate|page-medium|paper>
  </collection>
</initial>
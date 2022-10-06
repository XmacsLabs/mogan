<TeXmacs|2.1.3>

<style|<tuple|tmdoc|chinese|old-spacing|old-dots|old-lengths>>

<\body>
  <tmdoc-title|样式属性详述>

  每一种基础图形对象都有对应的一些影响渲染的样式属性。这些属性如下所示：

  <paragraph|颜色>该属性用于指定颜色，应用于任何图形对象。

  <paragraph|填充色>该属性用于指定对象的填充色，应用于所有图形对象，文本和数学除外。

  <\big-figure|<with|gr-mode|<tuple|edit|cspline>|gr-frame|<tuple|scale|1cm|<tuple|0.5gw|0.5gh>>|gr-geometry|<tuple|geometry|1par|0.6par>|gr-fill-color|pastel
  red|gr-color|dark magenta|gr-auto-crop|true|<graphics||<cspline|<point|-4.82503|-0.183391>|<point|-4.54985778542135|2.12379282973938>|<point|-3.34334898796137|1.57345548352957>|<point|-2.623677073687|0.070611191956608>|<point|-3.89368633417119|0.790283106230983>>|<with|fill-color|pastel
  yellow|<cspline|<point|-1.75584|2.22963>|<point|-2.24267429554174|1.23478634740045>|<point|-1.62883648630771|0.324613044053446>|<point|-1.88283833840455|1.21361952639238>>>|<with|color|dark
  blue|fill-color|pastel orange|<cspline|<point|-0.0201581|1.78512>|<point|-0.845664109009128|1.36178727344887>|<point|-0.464661330863871|0.493947612118005>|<point|0.424345151475063|0.917284032279402>>>|<with|color|dark
  magenta|fill-color|pastel red|<cspline|<point|2.11769|1.99679>|<point|2.81619592538696|0.811449927239053>|<point|1.56735348591083|0.769116285222913>>>>>>
    一些不同颜色和填充色的闭合样条曲线示例。
  </big-figure>

  <paragraph|不透明度>该属性用于指定对象从0%到100%的不透明度，也是应用于所有图形对象。默认的不透明度为100%，不透明度越低，对象就越透明。

  <big-figure|<with|gr-mode|<tuple|edit|carc>|gr-frame|<tuple|scale|1cm|<tuple|0.5gw|0.5gh>>|gr-geometry|<tuple|geometry|1par|0.6par>|gr-grid|<tuple|empty>|gr-grid-old|<tuple|cartesian|<point|0|0>|1>|gr-edit-grid-aspect|<tuple|<tuple|axes|none>|<tuple|1|none>|<tuple|10|none>>|gr-edit-grid|<tuple|empty>|gr-edit-grid-old|<tuple|cartesian|<point|0|0>|1>|gr-fill-color|pastel
  green|gr-color|dark green|gr-opacity|20%|gr-auto-crop|true|<graphics||<with|color|dark
  brown|fill-color|brown|<cline|<point|-5|1>|<point|-5.0|-1.0>|<point|5.0|-1.0>|<point|5.0|1.0>>>|<with|color|dark
  green|fill-color|pastel green|<carc|<point|4|-1.5>|<point|4.5|-1.0>|<point|4.0|-0.5>>>|<with|color|dark
  green|opacity|80%|fill-color|pastel green|<carc|<point|2|-1.5>|<point|2.5|-1.0>|<point|2.0|-0.5>>>|<with|color|dark
  green|opacity|60%|fill-color|pastel green|<carc|<point|0|-1.5>|<point|0.5|-1.0>|<point|0.0|-0.5>>>|<with|color|dark
  green|opacity|40%|fill-color|pastel green|<carc|<point|-2|-1.5>|<point|-1.5|-1.0>|<point|-2.0|-0.5>>>|<with|color|dark
  green|opacity|20%|fill-color|pastel green|<carc|<point|-4|-1.5>|<point|-3.5|-1.0>|<point|-4.0|-0.5>>>>>|覆盖在另一个对象上的同一个对象不透明度逐渐增加的一个过程的示例。>

  <paragraph|点形状>支持这些不同的形状：
  圆盘、圆圈和方框。

  <big-figure|<with|gr-mode|<tuple|edit|text-at>|gr-frame|<tuple|scale|1cm|<tuple|0.5gw|0.5gh>>|gr-geometry|<tuple|geometry|1par|0.6par>|gr-fill-color|red|gr-grid|<tuple|empty>|gr-grid-old|<tuple|cartesian|<point|0|0>|1>|gr-edit-grid-aspect|<tuple|<tuple|axes|none>|<tuple|1|none>|<tuple|10|none>>|gr-edit-grid|<tuple|empty>|gr-edit-grid-old|<tuple|cartesian|<point|0|0>|1>|gr-point-style|square|gr-text-at-halign|center|gr-auto-crop|true|<graphics||<with|fill-color|red|<point|-2|0>>|<with|fill-color|red|point-style|round|<point|0|0>>|<with|fill-color|red|point-style|square|<point|2|0>>|<with|text-at-halign|center|<text-at|方框|<point|2|0.5>>>|<with|text-at-halign|center|<text-at|圆圈|<point|0|0.5>>>|<with|text-at-halign|center|<text-at|圆盘|<point|-2|0.5>>>>>|黑色和红色填充色的不同的点形状。>

  <paragraph|线宽度>线宽度属性应用于所有曲线（包括线段，多边形，样条曲线，闭合样条曲线，圆弧和圆）。其默认大小为<verbatim|1ln>，即数学公式分式中线段的宽度。你可以指定其为任何<TeXmacs>长度。

  <\big-figure|<with|gr-mode|<tuple|edit|text-at>|gr-frame|<tuple|scale|1cm|<tuple|0.5gw|0.5gh>>|gr-geometry|<tuple|geometry|1par|0.6par>|gr-grid|<tuple|empty>|gr-grid-old|<tuple|cartesian|<point|0|0>|1>|gr-edit-grid-aspect|<tuple|<tuple|axes|none>|<tuple|1|none>|<tuple|10|none>>|gr-edit-grid|<tuple|empty>|gr-edit-grid-old|<tuple|cartesian|<point|0|0>|1>|gr-line-width|5ln|gr-auto-crop|true|<graphics||<spline|<point|-3|0>|<point|-2.0|0.3>|<point|0.0|-0.3>|<point|2.0|0.3>|<point|3.0|0.0>>|<with|line-width|0.5ln|<spline|<point|-3|1>|<point|-2.0|1.3>|<point|0.0|0.7>|<point|2.0|1.3>|<point|3.0|1.0>>>|<with|line-width|2ln|<spline|<point|-3|-1>|<point|-2.0|-0.7>|<point|0.0|-1.3>|<point|2.0|-0.7>|<point|3.0|-1.0>>>|<with|line-width|5ln|<spline|<point|-3|-2>|<point|-2.0|-1.7>|<point|0.0|-2.3>|<point|2.0|-1.7>|<point|3.0|-2.0>>>|<text-at|0。5ln|<point|3.5|1>>|<text-at|1ln|<point|3.5|0>>|<text-at|2ln|<point|3.5|-1>>|<text-at|5ln|<point|3.5|-2>>>>>
    使用不同线宽度的同一条曲线。
  </big-figure>

  <paragraph|线组成>可以在<menu|Focus|Line
  dashes>中指定曲线的组成样式。

  <\big-figure|<with|gr-mode|<tuple|edit|line>|gr-frame|<tuple|scale|1cm|<tuple|0.5gw|0.5gh>>|gr-geometry|<tuple|geometry|1par|0.6par>|gr-grid|<tuple|empty>|gr-grid-old|<tuple|cartesian|<point|0|0>|1>|gr-edit-grid-aspect|<tuple|<tuple|axes|none>|<tuple|1|none>|<tuple|10|none>>|gr-edit-grid|<tuple|empty>|gr-edit-grid-old|<tuple|cartesian|<point|0|0>|1>|gr-line-width|2ln|gr-dash-style|11100|gr-dash-style-unit|20ln|gr-auto-crop|true|<graphics||<with|line-width|2ln|<line|<point|-3|1>|<point|-2.0|1.5>|<point|0.0|0.5>|<point|2.0|1.5>|<point|3.0|1.0>>>|<with|dash-style|10|line-width|2ln|<line|<point|-3|0.5>|<point|-2.0|1.0>|<point|0.0|0.0>|<point|2.0|1.0>|<point|3.0|0.5>>>|<with|dash-style|11100|line-width|2ln|<line|<point|-3|0>|<point|-2.0|0.5>|<point|0.0|-0.5>|<point|2.0|0.5>|<point|3.0|0.0>>>|<with|dash-style|1111010|line-width|2ln|<line|<point|-3|-0.5>|<point|-2.0|0.0>|<point|0.0|-1.0>|<point|2.0|0.0>|<point|3.0|-0.5>>>|<with|dash-style|1111010|line-width|2ln|dash-style-unit|10ln|<line|<point|-3|-1>|<point|-2.0|-0.5>|<point|0.0|-1.5>|<point|2.0|-0.5>|<point|3.0|-1.0>>>|<with|dash-style|11100|line-width|2ln|dash-style-unit|20ln|<line|<point|-3|-1.5>|<point|-2.0|-1.0>|<point|0.0|-2.0>|<point|2.0|-1.0>|<point|3.0|-1.5>>>>>>
    使用不同组成样式的同一条曲线。
  </big-figure>

  <paragraph|线箭头>可以使用<menu|Focus|Line
  arrows>指定曲线两端的箭头样式。

  <\big-figure|<with|gr-mode|<tuple|edit|line>|gr-frame|<tuple|scale|1cm|<tuple|0.5gw|0.5gh>>|gr-geometry|<tuple|geometry|1par|0.6par>|gr-grid|<tuple|empty>|gr-grid-old|<tuple|cartesian|<point|0|0>|1>|gr-edit-grid-aspect|<tuple|<tuple|axes|none>|<tuple|1|none>|<tuple|10|none>>|gr-edit-grid|<tuple|empty>|gr-edit-grid-old|<tuple|cartesian|<point|0|0>|1>|gr-line-width|2ln|gr-arrow-begin|\||gr-arrow-end|\||gr-auto-crop|true|<graphics||<with|line-width|2ln|<line|<point|-4|2>|<point|-1.0|2.0>>>|<with|arrow-end|\<gtr\>|line-width|2ln|<line|<point|-4|1.5>|<point|-1.0|1.5>>>|<with|arrow-end|\|\<gtr\>|line-width|2ln|<line|<point|-4|1>|<point|-1.0|1.0>>>|<with|arrow-end|\<gtr\>\<gtr\>|line-width|2ln|<line|<point|-4|0.5>|<point|-1.0|0.5>>>|<with|arrow-end|\<less\>|line-width|2ln|<line|<point|-4|0>|<point|-1.0|0.0>>>|<with|arrow-end|\<less\>\||line-width|2ln|<line|<point|-4|-0.5>|<point|-1.0|-0.5>>>|<with|arrow-end|\<less\>\<less\>|line-width|2ln|<line|<point|-4|-1>|<point|-1.0|-1.0>>>|<with|arrow-end|\||line-width|2ln|<line|<point|-4|-1.5>|<point|-1.0|-1.5>>>|<with|arrow-end|o|line-width|2ln|<line|<point|-4|-2>|<point|-1.0|-2.0>>>|<with|arrow-end|\|\<gtr\>|line-width|2ln|arrow-begin|\<less\>\||<line|<point|0|2>|<point|3.0|2.0>>>|<with|arrow-end|\|\<gtr\>|line-width|2ln|arrow-begin|\<gtr\>|<line|<point|0|1.5>|<point|3.0|1.5>>>|<with|arrow-end|o|line-width|2ln|arrow-begin|o|<line|<point|0|1>|<point|3.0|1.0>>>|<with|arrow-end|\<less\>\||line-width|2ln|arrow-begin|\|\<gtr\>|<line|<point|0|0.5>|<point|3.0|0.5>>>|<with|arrow-end|\<less\>|line-width|2ln|arrow-begin|\<gtr\>|<line|<point|0|0>|<point|3.0|0.0>>>|<with|line-width|2ln|arrow-begin|\<gtr\>|<line|<point|0|-0.5>|<point|3.0|-0.5>>>|<with|arrow-end|\<gtr\>\<gtr\>|line-width|2ln|arrow-begin|\<gtr\>|<line|<point|0|-1>|<point|3.0|-1.0>>>|<with|arrow-end|\<gtr\>\<gtr\>|line-width|2ln|arrow-begin|\<gtr\>\<gtr\>|<line|<point|0|-1.5>|<point|3.0|-1.5>>>|<with|arrow-end|\||line-width|2ln|arrow-begin|\||<line|<point|0|-2>|<point|3.0|-2.0>>>>>>
    两端使用不同类型箭头的同一条线段。
  </big-figure>

  <paragraph|文本对齐>对于文本和数学方格，可以指定其水平和竖直对齐的属性，如下所示：

  <\big-figure|<with|gr-mode|<tuple|edit|text-at>|gr-frame|<tuple|scale|1cm|<tuple|0.420008gw|0.5gh>>|gr-geometry|<tuple|geometry|1par|0.6par>|gr-grid|<tuple|empty>|gr-grid-old|<tuple|cartesian|<point|0|0>|1>|gr-edit-grid-aspect|<tuple|<tuple|axes|none>|<tuple|1|none>|<tuple|10|none>>|gr-edit-grid|<tuple|empty>|gr-edit-grid-old|<tuple|cartesian|<point|0|0>|1>|gr-line-width|2ln|gr-text-at-halign|center|gr-text-at-valign|top|gr-auto-crop|true|<graphics||<with|color|light
  grey|line-width|2ln|<line|<point|-3|2>|<point|-3.0|-2.0>>>|<with|text-at-valign|center|<text-at|左边|<point|-3|1>>>|<with|text-at-valign|center|text-at-halign|center|<text-at|居中|<point|-3|0>>>|<with|text-at-valign|center|text-at-halign|right|<text-at|右边|<point|-3|-1>>>|<with|color|light
  grey|line-width|2ln|<line|<point|-1|0>|<point|7.0|0.0>>>|<with|text-at-valign|bottom|text-at-halign|center|<text-at|底部
  <math|y>|<point|0|0>>>|<with|text-at-halign|center|<text-at|基线
  <math|y>|<point|1.5|0>>>|<with|text-at-valign|axis|text-at-halign|center|<text-at|轴线
  <math|y>|<point|3|0>>>|<with|text-at-valign|center|text-at-halign|center|<text-at|居中
  <math|y>|<point|4.5|0>>>|<with|text-at-valign|top|text-at-halign|center|<text-at|顶部
  <math|y>|<point|6|0>>>>>>
    文本方格的水平和垂直方向上对齐的图解。
  </big-figure>

  \;

  <tmdoc-copyright|2012|Joris van der Hoeven>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>
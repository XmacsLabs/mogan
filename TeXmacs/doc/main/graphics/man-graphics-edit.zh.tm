<TeXmacs|2.1.3>

<style|<tuple|tmdoc|chinese|old-spacing|old-dots|old-lengths>>

<\body>
  <tmdoc-title|编辑图形对象>

  所有支持插入新对象（点，线，多边形等）的模式也同样支持直接编辑已存在的对象。一旦鼠标移动到一个对象上方，该对象的控制点就会被自动高亮，并一些编辑操作，如下所示：

  <\description>
    <item*|移动控制点>当鼠标足够接近一个控制点时，你可以用鼠标左键拖拽控制点到某处。

    <item*|插入新的控制点>对于那些拥有一个以上控制点的对象，比如线、多边形、样条曲线和闭合样条曲线，可以在边上插入新的控制点。首先将鼠标移动到目标边上，此时目标边两端的控制点将高亮显示。然后按下鼠标左键不放并拖拽，在合适的地方松开。

    <item*|移除控制点>使用鼠标中键可以移除当前控制点。也可以用<key|Del>或者<key|Backspace>。

    <item*|移除整个对象>使用鼠标中键并同时按下<key|S->将移除当前高亮的整个对象。
  </description>

  在编辑过程中，你可能注意到了，<TeXmacs>会试着自动将鼠标<em|附着>到控制点、对象的边缘、曲线的交点和网格上的点。这就使得我们能够快速绘制复杂的精确图形，不仅仅精确到一两个像素（指那种发大或者打印后很丑的精度）。在文本或者数学公式四周有八个隐藏的控制点，<TeXmacs>会尽可能地把鼠标附着到这些点上。这使得图<reference|diagram-fig>的绘制变得容易得多。

  图形对象在图片上的绘制依照一种栈的顺序，这种顺序会影响到重叠对象的覆盖和被覆盖。使用<shortcut|(graphics-zmove
  'closer)> 和<shortcut|(graphics-zmove 'farther)>，你可以使当前高亮的对象上浮或者下沉。同样地，控制点也可能由于被其它控制点覆盖而无法访问。在这种情况下，你可以使用<key|tab>遍历在当前光标所在位置下的所有控制点。

  <\big-figure|<with|gr-mode|<tuple|group-edit|group-ungroup>|gr-frame|<tuple|scale|1cm|<tuple|0.729978gw|0.140033gh>>|gr-geometry|<tuple|geometry|1par|0.6par>|gr-grid|<tuple|empty>|gr-grid-old|<tuple|cartesian|<point|0|0>|1>|gr-edit-grid-aspect|<tuple|<tuple|axes|none>|<tuple|1|none>|<tuple|10|none>>|gr-edit-grid|<tuple|empty>|gr-edit-grid-old|<tuple|cartesian|<point|0|0>|1>|gr-text-at-halign|center|gr-arrow-end|\<gtr\>|gr-auto-crop|true|<graphics||<with|text-at-halign|center|<math-at|A|<point|-5|3>>>|<with|text-at-halign|center|<math-at|B|<point|-3.5|3>>>|<with|text-at-halign|center|<math-at|C|<point|-5|2>>>|<with|text-at-halign|center|<math-at|D|<point|-3.5|2>>>|<with|text-at-halign|center|<math-at|X|<point|-2|3>>>|<with|text-at-halign|center|<math-at|Y|<point|-2|2>>>|<with|arrow-end|\<gtr\>|<line|<point|-4.75015|3.12052>|<point|-3.75133946289192|3.1205185871147>>>|<with|arrow-end|\<gtr\>|<line|<point|-5|2.88244>|<point|-5.0|2.35861225029766>>>|<with|arrow-end|\<gtr\>|<line|<point|-4.75637|2.12052>|<point|-3.76357653128721|2.1205185871147>>>|<with|arrow-end|\<gtr\>|<line|<point|-3.5|2.88244>|<point|-3.5|2.35861225029766>>>|<with|arrow-end|\<gtr\>|<line|<point|-3.24864|3.12052>|<point|-2.26367575076068|3.1205185871147>>>|<with|arrow-end|\<gtr\>|<line|<point|-3.23641|2.12052>|<point|-2.21995303611589|2.1205185871147>>>|<with|arrow-end|\<gtr\>|<line|<point|-2|2.88244>|<point|-2.0|2.35861225029766>>>|<with|arrow-end|\<gtr\>|<spline|<point|-4.75015|3.35861>|<point|-2.87767892578383|3.71332186797195>|<point|-1.31133417118666|3.26881862680249>|<point|-1.7800304273052|2.35861225029766>>>>>>
    <label|diagram-fig>使用数学公式方框四周八个控制点的附着特性作图的一个例子。注意该图已经被剪裁过了。
  </big-figure>

  <tmdoc-copyright|2012|Joris van der Hoeven>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>
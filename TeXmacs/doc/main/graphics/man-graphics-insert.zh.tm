<TeXmacs|2.1.3>

<style|<tuple|tmdoc|chinese|old-spacing|old-dots|old-lengths>>

<\body>
  <tmdoc-title|插入图形对象>

  插入新图片或点击进入已有图片后，依赖于模式的第二行工具栏会显示一系列图形模式的图标。特别地，其中的第二组图标<icon|tm_point_mode.xpm>，
  <icon|tm_line_mode.xpm>， <icon|tm_cline_mode.xpm>，
  <icon|tm_spline_mode.xpm>， <icon|tm_cspline_mode.xpm>，
  <icon|tm_arc_mode.xpm>， <icon|tm_carc_mode.xpm>，
  <icon|tm_textat_mode.xpm>， <icon|tm_math.xpm>是一系列可以点击并插入图片的图形对象。<TeXmacs>当前支持下列这些原生的图形对象：

  <\description>
    <item*|点>点击<icon|tm_point_mode.xpm>或者<menu|Insert|Point>，你就能在图片上通过鼠标左键插入点。

    <item*|线和多边形>点击<icon|tm_line_mode.xpm>或者<menu|Insert|Line>后，每一次左击都将插入一个虚拟的点，直线将顺次连接这些点，双击可以插入最后一个点，并结束本次绘制。点击<icon|tm_cline_mode.xpm>或者<menu|Insert|Polygon>后，行为与插入直线类似，只是最后一个点插入后，将有一条额外的直线使之与第一个点相连。

    <item*|样条曲线和闭合样条曲线><icon|tm_spline_mode.xpm>即<menu|Insert|Spline>，<icon|tm_cspline_mode.xpm>即<menu|Insert|Closed
    spline>。插入曲线的操作方式与插入直线的方式一模一样，不同之处在于，连接这些点的是样条曲线。

    <item*|圆弧和圆圈><icon|tm_arc_mode.xpm>即<menu|Insert|Arc>，<icon|tm_carc_mode.xpm>即<menu|Insert|Circle>。平面上不在同一直线上的三点可以确定一个圆。所以指定该对象后，只需要左击插入三个点即可。

    <item*|文本和数学><icon|tm_textat_mode.xpm>即<menu|Insert|Text>，<icon|tm_math.xpm>即<menu|Insert|Mathematics>。指定该对象后，在图片上左击选中插入的位置，再编辑即可。值得注意的是，这里的文本其实不限定于纯粹的文本，其本质是一个容器，大部分<TeXmacs>内容都可以栖身其中。
  </description>

  这些基本对象的典型例子如下图所示：

  <\center>
    <tabular*|<tformat|<table|<row|<cell|<small-figure|<with|gr-mode|point|gr-frame|<tuple|scale|1cm|<tuple|0.5gw|0.5gh>>|gr-geometry|<tuple|geometry|3cm|3cm|center>|<graphics||<point|0.499041|-0.00699497>|<point|-0.919136|-0.23983>|<point|0.329706|-1.19234>>>|点>>|<cell|<small-figure|<with|gr-mode|<tuple|edit|line>|gr-frame|<tuple|scale|1cm|<tuple|0.5gw|0.5gh>>|gr-geometry|<tuple|geometry|3cm|3cm|center>|<graphics||<line|<point|-0.926313|0.839678>|<point|0.809366318296071|0.564509194337875>|<point|-0.862812541341447|-0.13399589892843>|<point|1.04220134938484|-0.260996824976849>|<point|-0.121973806059003|-1.17117012832385>>>>|线>>|<cell|<small-figure|<with|gr-mode|<tuple|edit|cline>|gr-frame|<tuple|scale|1cm|<tuple|0.5gw|0.5gh>>|gr-geometry|<tuple|geometry|3cm|3cm|center>|<graphics||<cline|<point|-0.742988|0.839678>|<point|0.844523085064162|0.691510120386294>|<point|0.886856727080302|-0.769000529170525>|<point|-0.425486175420029|-1.2770042333642>|<point|-1.25099219473475|-0.13399589892843>>>>|线段>>>>>>

    \;

    <tabular*|<tformat|<table|<row|<cell|<small-figure|<with|gr-mode|<tuple|edit|spline>|gr-frame|<tuple|scale|1cm|<tuple|0.5gw|0.5gh>>|gr-geometry|<tuple|geometry|3cm|3cm|center>|<graphics||<spline|<point|-1.1308|1.0844>|<point|0.880043656568329|0.661066278608281>|<point|-0.855635666093399|-0.0374388146580235>|<point|0.901210477576399|-0.48194205582749>|<point|-0.516966529964281|-1.1804471490938>>>>|样条曲线>>|<cell|<small-figure|<with|gr-mode|<tuple|edit|cspline>|gr-frame|<tuple|scale|1cm|<tuple|0.5gw|0.5gh>>|gr-geometry|<tuple|geometry|3cm|3cm|center>|<graphics||<cspline|<point|-0.651144|0.788067>|<point|0.89403360232835|0.322397142479164>|<point|0.640031750231512|-0.905278475988887>|<point|-0.566477047228469|-1.20161397010187>|<point|-1.15914803545443|-0.206773382722582>>>>|闭合样条曲线>>|<cell|<small-figure|<with|gr-mode|<tuple|edit|arc>|gr-frame|<tuple|scale|1cm|<tuple|0.5gw|0.5gh>>|gr-geometry|<tuple|geometry|3cm|3cm|center>|<graphics||<arc|<point|-0.827656|-1.15928>|<point|0.865689906072232|-0.5454425188517>|<point|-0.806488953565286|0.87273448868898>>>>|圆弧>>>>>>

    \;

    <tabular*|<tformat|<table|<row|<cell|<small-figure|<with|gr-mode|<tuple|edit|carc>|gr-frame|<tuple|scale|1cm|<tuple|0.5gw|0.5gh>>|gr-geometry|<tuple|geometry|3cm|3cm|center>|<graphics||<carc|<point|-0.262964677867443|0.990458393967456>|<point|1.02821140362482|-0.0467158354279667>|<point|-0.0724632887948141|-1.1473905278476>>>>|圆圈>>|<cell|<small-figure|<with|gr-mode|<tuple|edit|text-at>|gr-frame|<tuple|scale|1cm|<tuple|0.5gw|0.5gh>>|gr-geometry|<tuple|geometry|3cm|3cm|center>|<graphics||<text-at|Hello|<point|-0.164307|0.545955>>>>|文本>>|<cell|<small-figure|<with|gr-mode|<tuple|edit|math-at>|gr-frame|<tuple|scale|1cm|<tuple|0.5gw|0.5gh>>|gr-geometry|<tuple|geometry|3cm|3cm|center>|<graphics||<math-at|\<mathe\><rsup|\<mathpi\>*\<mathi\>>=-1|<point|-1.08166|-0.533553>>>>|数学>>>>>>
  </center>

  <htab|5mm>

  <tmdoc-copyright|2012|Joris van der Hoeven>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>
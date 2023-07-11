<TeXmacs|2.1.2>

<style|<tuple|tmdoc|british|devel>>

<\body>
  <tmdoc-title|Mogan Draw <code|optimization> (OSPP)>

  This is a project about the drawing function of the Mogan Editor. It is
  also a project for the OpenSource Summer of Code.

  <section|Target>

  The main goal of this project is to improve the built-in tools for making
  technical graphics to be friendly enough for most users. User-friendliness
  includes many aspects, such as rich drawing tools, easy-to-open WASM web
  application (easier to use than local applications), reasonable UI
  interface, less bugs, comfortable interaction methods, etc. The focus of
  this project is to improve the user experience.

  <\quotation>
    Imagine that you are a teacher in a junior high school teaching Euclid's
    Elements, and you are using Mogan Editor/GNU TeXmacs to prepare the
    slides. You should be able to re-draw the figures in Euclid's Element via
    Mogan Draw in an easy way (straight-forward and no need to remember any
    tricks) and can persuade your colleagues to use Mogan Draw.
  </quotation>

  For example, in the above scenario, we hope that the improved mogan draw
  can meet the needs of middle school teachers and be easy to use.

  <section|Why>

  Due to the fact that<nbsp>Mogan Editor<nbsp>targets a user base that
  includes primary and secondary school teachers, and<nbsp>drawing
  functions<nbsp>are commonly used by them, we need to optimize the product
  to improve user experience.

  <section|Tasks>

  <\wide-tabular>
    <tformat|<cwith|2|2|4|4|cell-valign|b>|<table|<row|<\cell>
      Feature
    </cell>|<\cell>
      23_1
    </cell>|<\cell>
      Oyyko
    </cell>|<\cell>
      Draw an ellipse. <href|https://gitee.com/XmacsLabs/mogan/issues/I6QBAE>
    </cell>>|<row|<\cell>
      Feature
    </cell>|<\cell>
      23_2
    </cell>|<\cell>
      Oyyko
    </cell>|<\cell>
      Draw sectors
    </cell>>|<row|<\cell>
      Feature
    </cell>|<\cell>
      23_3
    </cell>|<\cell>
      Oyyko
    </cell>|<\cell>
      add the Snap mode to keep the lines horizontal, vertical or parallel to
      existing line segments. <href|https://gitee.com/XmacsLabs/mogan/issues/I4WURX>
    </cell>>|<row|<\cell>
      Feature
    </cell>|<\cell>
      23_4
    </cell>|<\cell>
      Oyyko
    </cell>|<\cell>
      Draw angle markers
    </cell>>|<row|<\cell>
      Feature
    </cell>|<\cell>
      23_5
    </cell>|<\cell>
      Oyyko
    </cell>|<\cell>
      Paint specific areas
    </cell>>|<row|<\cell>
      Feature
    </cell>|<\cell>
      23_6
    </cell>|<\cell>
      Oyyko
    </cell>|<\cell>
      Add more arrow types. <href|https://gitee.com/XmacsLabs/mogan/issues/I6BKQN>
    </cell>>|<row|<\cell>
      Feature
    </cell>|<\cell>
      23_7
    </cell>|<\cell>
      Oyyko
    </cell>|<\cell>
      After pressing Shift, pressing the left mouse button is equivalent to
      pressing the right mouse button. <href|https://gitee.com/XmacsLabs/mogan/issues/I5XGW8>
    </cell>>|<row|<\cell>
      Feature
    </cell>|<\cell>
      23_8
    </cell>|<\cell>
      Oyyko
    </cell>|<\cell>
      Enhance the practicability of endpoint snapping in the drawing
      function. <href|https://gitee.com/XmacsLabs/mogan/issues/I5IC8N>
    </cell>>|<row|<\cell>
      Feature
    </cell>|<\cell>
      23_9
    </cell>|<\cell>
      Oyyko
    </cell>|<\cell>
      Drawing object alignment and equidistant distribution functions
    </cell>>|<row|<\cell>
      Feature
    </cell>|<\cell>
      23_10
    </cell>|<\cell>
      Oyyko
    </cell>|<\cell>
      Interchange graph exportable TikZ code.
      <href|https://gitee.com/XmacsLabs/mogan/issues/I6W4GJ>
    </cell>>|<row|<\cell>
      Bug
    </cell>|<\cell>
      23_11
    </cell>|<\cell>
      Oyyko
    </cell>|<\cell>
      When pasting an object, it is necessary to keep a certain distance from
      the original object. <href|https://gitee.com/XmacsLabs/mogan/issues/I5XGKD>
    </cell>>|<row|<\cell>
      Bug
    </cell>|<\cell>
      23_12
    </cell>|<\cell>
      Oyyko
    </cell>|<\cell>
      Select a drawing, and then export to PDF or PNG, the curve becomes not
      smooth. <href|https://gitee.com/XmacsLabs/mogan/issues/I62I5H>
    </cell>>|<row|<\cell>
      Bug
    </cell>|<\cell>
      23_13
    </cell>|<\cell>
      Oyyko
    </cell>|<\cell>
      After selecting an object, using the cursor should not move the
      background, but the object. <href|https://gitee.com/XmacsLabs/mogan/issues/I5XGQ7>
    </cell>>|<row|<\cell>
      Feature
    </cell>|<\cell>
      23_14
    </cell>|<\cell>
      Oyyko
    </cell>|<\cell>
      After entering the function of moving, the cursor should become a small
      hand. <href|https://gitee.com/XmacsLabs/mogan/issues/I5XGM7>
    </cell>>|<row|<\cell>
      Feature
    </cell>|<\cell>
      23_15
    </cell>|<\cell>
      Oyyko
    </cell>|<\cell>
      When inserting a Text At object, the alignment of Centre/Centre should
      be adopted by default. <href|https://gitee.com/XmacsLabs/mogan/issues/I5XGVX>
      and <href|https://github.com/XmacsLabs/mogan/issues/185>
    </cell>>|<row|<\cell>
      Bug
    </cell>|<\cell>
      23_16
    </cell>|<\cell>
      Oyyko
    </cell>|<\cell>
      When moving an object, the snap point may not be the object's vertex.
      <href|https://gitee.com/XmacsLabs/mogan/issues/I668I5>
    </cell>>|<row|<\cell>
      Bug
    </cell>|<\cell>
      23_17
    </cell>|<\cell>
      Oyyko
    </cell>|<\cell>
      After reducing the image, the input box will not shrink accordingly.
      <href|https://gitee.com/XmacsLabs/mogan/issues/I66QRB>
    </cell>>|<row|<\cell>
      Bug
    </cell>|<\cell>
      23_18
    </cell>|<\cell>
      Oyyko
    </cell>|<\cell>
      Text on arrow with text cannot be used as macro variable.
      <href|https://gitee.com/XmacsLabs/mogan/issues/I689L0>
    </cell>>>>
  </wide-tabular>

  \;
</body>

<\initial>
  <\collection>
    <associate|page-medium|papyrus>
  </collection>
</initial>
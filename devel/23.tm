<TeXmacs|2.1.2>

<style|<tuple|tmdoc|british|devel>>

<\body>
  <tmdoc-title|Mogan Draw <code|optimization> (OSPP)>

  This is a project about the drawing function of the Mogan Editor. It is
  also a project for the OpenSource Summer of Code. The duration for this
  OSPP is from July to September in 2023.

  \;

  <section|Goal>

  The main goal of this project is to improve the built-in tools for making
  technical graphics to be friendly enough for most users. User-friendliness
  includes many aspects, such as rich drawing tools, easy-to-open WASM web
  application (easier to use than local applications), reasonable UI
  interface, less bugs, comfortable interaction methods, etc. The focus of
  this project is to improve the user experience.

  <\quotation>
    Imagine that you are a teacher in a junior high school teaching Euclid's
    Elements, and you are using Mogan Editor/GNU <TeXmacs> to prepare the
    slides. You should be able to re-draw the figures in Euclid's Element via
    Mogan Draw in an easy way (straight-forward and no need to remember any
    tricks) and can persuade your colleagues to use Mogan Draw.
  </quotation>

  For example, in the above scenario, we hope that the improved mogan draw
  can meet the needs of middle school teachers and be easy to use.

  <section|Why>

  Due to the fact that Mogan Editor targets a user base that includes primary
  and secondary school teachers, and drawing functions are commonly used by
  them, we need to optimize the product to improve user experience.\ 

  <section|Tasks>

  <\wide-block>
    <tformat|<table|<row|<\cell>
      Type
    </cell>|<\cell>
      No.
    </cell>|<\cell>
      Mgr.
    </cell>|<\cell>
      Description
    </cell>|<\cell>
      Links
    </cell>|<\cell>
      Status
    </cell>>|<row|<\cell>
      <value|f>
    </cell>|<\cell>
      <dlink|23_1>
    </cell>|<\cell>
      <value|oy>
    </cell>|<\cell>
      Draw an ellipse.
    </cell>|<\cell>
      <href|https://gitee.com/XmacsLabs/mogan/issues/I6QBAE>
    </cell>|<\cell>
      <value|re>
    </cell>>|<row|<\cell>
      <value|b>
    </cell>|<\cell>
      <dlink|23_2>
    </cell>|<\cell>
      <value|oy>
    </cell>|<\cell>
      When moving an object, the snap point may not be the object's vertex.
    </cell>|<\cell>
      <href|https://gitee.com/XmacsLabs/mogan/issues/I668I5>, just a ref, the
      content of this bug will be explained in <dlink|23_2>.
    </cell>|<\cell>
      <value|li>

      \;
    </cell>>|<row|<\cell>
      <value|b>
    </cell>|<\cell>
      <dlink|23_3>
    </cell>|<\cell>
      <value|oy>
    </cell>|<\cell>
      Select a drawing, and then export to PDF or PNG, the curve becomes not
      smooth.
    </cell>|<\cell>
      <href|https://gitee.com/XmacsLabs/mogan/issues/I62I5H>
    </cell>|<\cell>
      <value|dis>
    </cell>>|<row|<\cell>
      <value|b>
    </cell>|<\cell>
      <dlink|23_4>
    </cell>|<\cell>
      <value|oy>
    </cell>|<\cell>
      After selecting an object, using the cursor should not move the
      background, but the object.
    </cell>|<\cell>
      <href|https://gitee.com/XmacsLabs/mogan/issues/I5XGQ7>
    </cell>|<\cell>
      <value|dis>
    </cell>>|<row|<\cell>
      <value|b>
    </cell>|<\cell>
      23_5
    </cell>|<\cell>
      <value|oy>
    </cell>|<\cell>
      After reducing the image, the input box will not shrink accordingly.
    </cell>|<\cell>
      <href|https://gitee.com/XmacsLabs/mogan/issues/I66QRB>
    </cell>|<\cell>
      <value|li>
    </cell>>|<row|<\cell>
      <value|f>
    </cell>|<\cell>
      <dlink|23_6>
    </cell>|<\cell>
      <value|oy>
    </cell>|<\cell>
      Add more arrow types.
    </cell>|<\cell>
      <href|https://gitee.com/XmacsLabs/mogan/issues/I6BKQN>
    </cell>|<\cell>
      <value|re>
    </cell>>|<row|<\cell>
      <value|f>
    </cell>|<\cell>
      <dlink|23_7>
    </cell>|<\cell>
      <value|oy>
    </cell>|<\cell>
      After pressing Shift, pressing the left mouse button is equivalent to
      pressing the right mouse button.
    </cell>|<\cell>
      <href|https://gitee.com/XmacsLabs/mogan/issues/I5XGW8>
    </cell>|<\cell>
      <value|ok>
    </cell>>|<row|<\cell>
      <value|f>
    </cell>|<\cell>
      23_8
    </cell>|<\cell>
      <value|oy>
    </cell>|<\cell>
      After entering the move mode, auto unselect the previously selected
      items.
    </cell>|<\cell>
      \;
    </cell>|<\cell>
      <value|li>
    </cell>>|<row|<\cell>
      <value|f>
    </cell>|<\cell>
      23_9
    </cell>|<\cell>
      <value|oy>
    </cell>|<\cell>
      Optimize the translation
    </cell>|<\cell>
      \;
    </cell>|<\cell>
      <value|li>
    </cell>>|<row|<\cell>
      <value|b>
    </cell>|<\cell>
      <dlink|23_11>
    </cell>|<\cell>
      <value|oy>
    </cell>|<\cell>
      When pasting an object, it is necessary to keep a certain distance from
      the original object.
    </cell>|<\cell>
      <href|https://gitee.com/XmacsLabs/mogan/issues/I5XGKD>
    </cell>|<\cell>
      <value|ok>
    </cell>>|<row|<\cell>
      <value|f>
    </cell>|<\cell>
      23_12
    </cell>|<\cell>
      <value|oy>
    </cell>|<\cell>
      Move the \PExit graphics mode\Q button to the rightest.
    </cell>|<\cell>
      \;
    </cell>|<\cell>
      <value|li>
    </cell>>|<row|<\cell>
      <value|f>
    </cell>|<\cell>
      23_13
    </cell>|<\cell>
      <value|oy>
    </cell>|<\cell>
      Change the Arrow setting memory from global to per-tool.
    </cell>|<\cell>
      \;
    </cell>|<\cell>
      <value|li>
    </cell>>|<row|<\cell>
      <value|f>
    </cell>|<\cell>
      <dlink|23_14>
    </cell>|<\cell>
      <value|oy>
    </cell>|<\cell>
      After entering the function of moving, the cursor should become a small
      hand.
    </cell>|<\cell>
      <href|https://gitee.com/XmacsLabs/mogan/issues/I5XGM7>
    </cell>|<\cell>
      <value|ok>
    </cell>>|<row|<\cell>
      <value|f>
    </cell>|<\cell>
      <dlink|23_15>
    </cell>|<\cell>
      <value|oy>
    </cell>|<\cell>
      When inserting a Text At object, the alignment of Centre/Centre should
      be adopted by default.
    </cell>|<\cell>
      <href|https://gitee.com/XmacsLabs/mogan/issues/I5XGVX> and
      <href|https://github.com/XmacsLabs/mogan/issues/185>
    </cell>|<\cell>
      <value|dis>
    </cell>>|<row|<\cell>
      <value|f>
    </cell>|<\cell>
      23_16
    </cell>|<\cell>
      <value|oy>
    </cell>|<\cell>
      Add the font size property for Text-at and normal text. Use
      <cpp|\\small> rather than size number to set the font size.
    </cell>|<\cell>
      \;
    </cell>|<\cell>
      <value|li>
    </cell>>|<row|<\cell>
      <value|f>
    </cell>|<\cell>
      23_17
    </cell>|<\cell>
      <value|oy>
    </cell>|<\cell>
      <strong|Bold> the origin point and the axes.
    </cell>|<\cell>
      \;
    </cell>|<\cell>
      <value|li>
    </cell>>|<row|<\cell>
      <value|f>
    </cell>|<\cell>
      23_18
    </cell>|<\cell>
      <value|oy>
    </cell>|<\cell>
      Display the coordinates of a point or object.
    </cell>|<\cell>
      \;
    </cell>|<\cell>
      <value|li>
    </cell>>|<row|<\cell>
      <value|f>
    </cell>|<\cell>
      23_19
    </cell>|<\cell>
      <value|oy>
    </cell>|<\cell>
      Set the location of an object by setting its coordinates.
    </cell>|<\cell>
      \;
    </cell>|<\cell>
      <value|li>
    </cell>>|<row|<\cell>
      <value|b>
    </cell>|<\cell>
      23_20
    </cell>|<\cell>
      <value|oy>
    </cell>|<\cell>
      Click the current mode to exit the current mode.
    </cell>|<\cell>
      \;
    </cell>|<\cell>
      <value|li>
    </cell>>|<row|<\cell>
      <value|b>
    </cell>|<\cell>
      23_21
    </cell>|<\cell>
      <value|oy>
    </cell>|<\cell>
      For Array-with-texts in two graphics area, if you edit one, the cursor
      will jump to the other one.
    </cell>|<\cell>
      \;
    </cell>|<\cell>
      <value|li>
    </cell>>|<\row|<\cell>
      <value|b>
    </cell>|<\cell>
      23_22
    </cell>|<\cell>
      <value|oy>
    </cell>|<\cell>
      For commutative diagram, the color-of-units should be setted by
      default.
    </cell>|<\cell>
      \;
    </cell>>
      <value|li>
    </row>|<row|<\cell>
      <value|f>
    </cell>|<\cell>
      <dlink|23_23>
    </cell>|<\cell>
      <value|oy>
    </cell>|<\cell>
      By default, draw a circle with two points. And change it to three
      points in the graphics-markup.
    </cell>|<\cell>
      \;
    </cell>|<\cell>
      <value|re>
    </cell>>|<row|<\cell>
      <strong|Code>
    </cell>|<\cell>
      23_24
    </cell>|<\cell>
      <value|oy>
    </cell>|<\cell>
      new complete cursor style setting function
    </cell>|<\cell>
      \;
    </cell>|<\cell>
      <value|ok>
    </cell>>>>
  </wide-block>

  \;

  \;

  \;
</body>

<\initial>
  <\collection>
    <associate|page-medium|papyrus>
  </collection>
</initial>
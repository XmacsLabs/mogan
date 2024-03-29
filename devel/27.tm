<TeXmacs|2.1.2>

<style|<tuple|tmdoc|british|devel>>

<\body>
  <tmdoc-title|Mogan Draw <code|optimization> (long-term)>

  This is a project about the improvement of the graphics mode of the Mogan
  Editor. Unlike <dlink|23>, this is a long-term task. Since Project
  <dlink|23> is an OSPP project and the summer time in 2023 is limited, we
  will put the unfinished tasks during the summer here.\ 

  In addition, this project will also serve as the primary project for
  improving the graphics mode in the future.

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

  \;

  <section|Why>

  Due to the fact that Mogan Editor<nbsp>targets a user base that includes
  primary and secondary school teachers, and drawing functions<nbsp>are
  commonly used by them, we need to optimize the product to improve the user
  experience.

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
      27_1
    </cell>|<\cell>
      <value|oy>
    </cell>|<\cell>
      Draw sectors
    </cell>|<\cell>
      \;
    </cell>|<\cell>
      <value|li>
    </cell>>|<row|<\cell>
      <value|f>
    </cell>|<\cell>
      27_2
    </cell>|<\cell>
      <value|oy>
    </cell>|<\cell>
      add the Snap mode to keep the lines horizontal, vertical or parallel to
      existing line segments.
    </cell>|<\cell>
      <href|https://gitee.com/XmacsLabs/mogan/issues/I4WURX>
    </cell>|<\cell>
      <value|li>
    </cell>>|<row|<\cell>
      <value|f>
    </cell>|<\cell>
      27_3
    </cell>|<\cell>
      <value|oy>
    </cell>|<\cell>
      Interchange graph exportable TikZ code.
    </cell>|<\cell>
      <href|https://gitee.com/XmacsLabs/mogan/issues/I6W4GJ>
    </cell>|<\cell>
      <value|li>
    </cell>>|<row|<\cell>
      <value|b>
    </cell>|<\cell>
      27_4
    </cell>|<\cell>
      <value|oy>
    </cell>|<\cell>
      Select a drawing, and then export to PDF or PNG, the curve becomes not
      smooth.
    </cell>|<\cell>
      \;
    </cell>|<\cell>
      <value|li>
    </cell>>|<row|<\cell>
      <value|b>
    </cell>|<\cell>
      27_5
    </cell>|<\cell>
      <value|oy>
    </cell>|<\cell>
      Text on arrow with text cannot be used as macro variable.
    </cell>|<\cell>
      <href|https://gitee.com/XmacsLabs/mogan/issues/I689L0>
    </cell>|<\cell>
      <value|li>
    </cell>>|<row|<\cell>
      <value|b>
    </cell>|<\cell>
      27_6
    </cell>|<\cell>
      <value|oy>
    </cell>|<\cell>
      For a red triangle, if we set the opacity to 50%, then there will be a
      strange area around the three lines. Maybe we should set the opacity of
      line and inner area separately.
    </cell>|<\cell>
      \;
    </cell>|<\cell>
      <value|li>
    </cell>>|<row|<\cell>
      <value|f>
    </cell>|<\cell>
      27_7
    </cell>|<\cell>
      <value|oy>
    </cell>|<\cell>
      The arrow snapping function. The arrow should snap on an object and
      when the object is moved, the arrow should follow.
    </cell>|<\cell>
      \;
    </cell>|<\cell>
      <value|li>
    </cell>>|<row|<\cell>
      <value|f>
    </cell>|<\cell>
      27_8
    </cell>|<\cell>
      <value|oy>
    </cell>|<\cell>
      Show the blank Text-at object.
    </cell>|<\cell>
      \;
    </cell>|<\cell>
      <value|li>
    </cell>>|<row|<\cell>
      <value|b>
    </cell>|<\cell>
      27_9
    </cell>|<\cell>
      <value|oy>
    </cell>|<\cell>
      The size of the blue box of the Text-at should change after the size of
      canvas is changed.
    </cell>|<\cell>
      \;
    </cell>|<\cell>
      <value|li>
    </cell>>|<row|<\cell>
      <value|f>
    </cell>|<\cell>
      27_10
    </cell>|<\cell>
      <value|oy>
    </cell>|<\cell>
      Mark the triangle or other object with Text A,B,C. We can set these
      texts as the property of the object rather than using the Text-at
      object.
    </cell>|<\cell>
      \;
    </cell>|<\cell>
      <value|li>
    </cell>>>>
  </wide-block>

  \;
</body>

<\initial>
  <\collection>
    <associate|page-medium|papyrus>
  </collection>
</initial>
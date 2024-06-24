<TeXmacs|2.1.2>

<style|<tuple|tmdoc|chinese|devel>>

<\body>
  <tmdoc-title|After pressing Shift, pressing the left mouse button is
  equivalent to pressing the right mouse button.>

  This is a <strong|Feature> of <with|font-series|bold|project 23>.\ 

  <section|Feature metadata>

  <\description>
    <item*|Owner>Oyyko

    <item*|Gitee issue><href|https://gitee.com/XmacsLabs/mogan/issues/I5XGW8>

    <item*|Github issue>NA
  </description>

  <section|Description>

  <subsection|Feature Description>

  Pressing the left mouse button after pressing shift should be equal to
  pressing the right mouse button.

  <subsection|Reason>

  In some special devices or special situations, it is difficult to press the
  right mouse button. For example using a touchpad.

  <subsection|Noteworthy points>

  Investigate whether shift has other uses. Will there be a conflict?

  <section|Planned implementation method>

  After pressing the shift key, it enters a new state. After entering this
  state, pressing other keys will exit this state, and pressing the left
  mouse button will trigger the event of pressing the right mouse button.

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
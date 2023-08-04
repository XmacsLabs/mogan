<TeXmacs|2.1.2>

<style|<tuple|tmdoc|chinese|devel>>

<\body>
  <tmdoc-title|Fix corrupted title in Html export>

  <section|Bug metadata>

  <\description>
    <item*|Reporter>jingkaimori

    <item*|Owner>jingkaimori

    <item*|Savannah issue><hlink|64258|https://savannah.gnu.org/bugs/index.php?64258>
  </description>

  <section|Description>

  This bug occurs when exporting an document containing images with non-latin
  name. Src field of image label in exported document is corruped, because
  scheme code of html exporting treat url object as usual string.

  <tmdoc-copyright|2022|jingkaimori>

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
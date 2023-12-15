<TeXmacs|2.1.2>

<style|<tuple|tmdoc|chinese|devel>>

<\body>
  <tmdoc-title|Certain semantic selections crash the editor>

  <section|Bug Metadata>

  <\description>
    <item*|Reporter>shostory on <hlink|savannah|https://savannah.gnu.org/bugs/?61628>

    <item*|Owner>Joris

    <item*|Savannah revision><svn|14276>
  </description>

  <section|How to reproduce it>

  Enable <menu|Preferrences for editing mathematical formulas|semantic
  selections>. Put the cursor between <math|a> and <math|\<Rightarrow\>> in
  beneath formula, then press <shortcut|(kbd-select kbd-right)>, i.e. select
  =\<gtr\>. it will freeze Mogan Research.

  <\equation*>
    a\<Rightarrow\>b
  </equation*>

  <tmdoc-copyright|2023|Jingkaimori>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<initial|<\collection>
</collection>>
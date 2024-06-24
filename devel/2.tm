<TeXmacs|2.1.2>

<style|<tuple|tmdoc|devel>>

<\body>
  <tmdoc-title|S7 Integration>

  For Mogan Editor 1.2.x, we will use S7 Scheme as a replacement of GNU Guile
  1.8.x.

  <section|Tasks>

  <\wide-tabular>
    <tformat|<cwith|1|16|2|2|cell-background|pastel
    green>|<cwith|17|17|2|2|cell-background|pastel green>|<table|<row|<\cell>
      Chore
    </cell>|<\cell>
      2_1
    </cell>|<\cell>
      Darcy
    </cell>|<\cell>
      Initial import of Massimiliano Gubinelli's work on S7 Scheme
    </cell>>|<row|<\cell>
      Chore
    </cell>|<\cell>
      2_2
    </cell>|<\cell>
      Darcy
    </cell>|<\cell>
      Deprecate Tiny Scheme integration
    </cell>>|<row|<\cell>
      Chore
    </cell>|<\cell>
      2_3
    </cell>|<\cell>
      Darcy
    </cell>|<\cell>
      Deprecate GNU Guile integration
    </cell>>|<row|<\cell>
      Bug
    </cell>|<\cell>
      <dlink|2_4>
    </cell>|<\cell>
      Darcy
    </cell>|<\cell>
      S7: procedure-symbol-name
    </cell>>|<row|<\cell>
      Chore
    </cell>|<\cell>
      2_5
    </cell>|<\cell>
      Darcy
    </cell>|<\cell>
      S7: disable benchmark-menu-expend
    </cell>>|<row|<\cell>
      Chore
    </cell>|<\cell>
      2_6
    </cell>|<\cell>
      Darcy
    </cell>|<\cell>
      S7: fix with-global by mgubi
    </cell>>|<row|<\cell>
      Chore
    </cell>|<\cell>
      2_7
    </cell>|<\cell>
      Darcy
    </cell>|<\cell>
      S7: Fix hang of shortcut editor
    </cell>>|<row|<\cell>
      Chore
    </cell>|<\cell>
      2_8
    </cell>|<\cell>
      Darcy
    </cell>|<\cell>
      S7: activate keyboard in chinese doc
    </cell>>|<row|<\cell>
      Chore
    </cell>|<\cell>
      2_9
    </cell>|<\cell>
      Darcy
    </cell>|<\cell>
      S7: Fix Bibtex related S7 Scheme bugs
    </cell>>|<row|<\cell>
      Chore
    </cell>|<\cell>
      2_10
    </cell>|<\cell>
      Darcy
    </cell>|<\cell>
      S7: implement iota
    </cell>>|<row|<\cell>
      Chore
    </cell>|<\cell>
      2_11
    </cell>|<\cell>
      Darcy
    </cell>|<\cell>
      S7: adopt hex character literal
    </cell>>|<row|<\cell>
      Chore
    </cell>|<\cell>
      2_12
    </cell>|<\cell>
      Darcy
    </cell>|<\cell>
      S7: compute-interactive-args failed for lazy-define, just use-modules
      to fix doc search
    </cell>>|<row|<\cell>
      Feature
    </cell>|<\cell>
      <dlink|2_13>
    </cell>|<\cell>
      Darcy
    </cell>|<\cell>
      S7: upgrade from 9.12 to 10.5
    </cell>>|<row|<\cell>
      Feature
    </cell>|<\cell>
      <dlink|2_14>
    </cell>|<\cell>
      jingkaimori
    </cell>|<\cell>
      Use lua to generate s7 glue
    </cell>>|<row|<\cell>
      Feature
    </cell>|<\cell>
      <dlink|2_15>
    </cell>|<\cell>
      jingkaimori
    </cell>|<\cell>
      Optimize generated s7 glue
    </cell>>|<row|<\cell>
      Bug
    </cell>|<\cell>
      <dlink|2_16>
    </cell>|<\cell>
      jingkaimori
    </cell>|<\cell>
      Use procedures defined in S7 to look up metadata instead of procedures
      defined in Guile.
    </cell>>|<row|<\cell>
      Chore
    </cell>|<\cell>
      2_17
    </cell>|<\cell>
      Darcy
    </cell>|<\cell>
      Remove legacy glue generator based on guile
    </cell>>>>
  </wide-tabular>

  <tmdoc-copyright|2023|Darcy Shen>

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
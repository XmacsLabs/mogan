<TeXmacs|2.1.2>

<style|<tuple|tmdoc|devel>>

<\body>
  <tmdoc-title|Unicode related issue>

  <TeXmacs> uses cork encoding to handle unicode string, which introduce a
  lot of trouble. We should examine all usage of string, and determine
  encoding of each string.

  <section|Tasks>

  <\wide-tabular>
    <tformat|<table|<row|<\cell>
      Bug
    </cell>|<\cell>
      <dlink|17_1>
    </cell>|<\cell>
      jingkaimori
    </cell>|<\cell>
      Fix incorrect word count for cjk content
    </cell>>|<row|<\cell>
      Chore
    </cell>|<\cell>
      <dlink|17_2>
    </cell>|<\cell>
      jingkaimori
    </cell>|<\cell>
      <menu|Tools|Statistics|Count Words> should be disabled for CJK
      documents
    </cell>>>>
  </wide-tabular>

  <tmdoc-copyright|2023|jingkaimori>

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
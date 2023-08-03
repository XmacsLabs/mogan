<TeXmacs|2.1.2>

<style|<tuple|tmdoc|devel|british>>

<\body>
  <tmdoc-title|Handle indentation of first paragraph of section>

  First paragraph of a section may not be indented in English document, but
  must be indented in Chinese document. <TeXmacs> uses <src-var|par-no-first>
  to control such behavior, but this environment variable is marked as
  internal in its document, therefore no gui is provided for user to toggle
  this boolean variable.

  <section|Tasks>

  <\wide-tabular>
    <tformat|<cwith|1|-1|2|2|cell-background|>|<table|<row|<\cell>
      Bug
    </cell>|<\cell>
      <dlink|46_1>
    </cell>|<\cell>
      jingkaimori
    </cell>|<\cell>
      Default value of <src-var|par-no-first> does not match default behavior
    </cell>>|<row|<\cell>
      Feature
    </cell>|<\cell>
      <dlink|46_2>
    </cell>|<\cell>
      jingkaimori
    </cell>|<\cell>
      Set <src-var|par-no-first> for paragraph in <menu|Format|Paragraph>
      menu
    </cell>>|<row|<\cell>
      Feature
    </cell>|<\cell>
      <todo|46_3>
    </cell>|<\cell>
      \;
    </cell>|<\cell>
      Set <src-var|par-no-first> for document in <menu|Format|Paragraph> menu
      and <menu|page layout> toolbar
    </cell>>|<row|<\cell>
      Doc
    </cell>|<\cell>
      46_4<rsub|>
    </cell>|<\cell>
      \;
    </cell>|<\cell>
      Improve documet of <src-var|par-no-first>
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
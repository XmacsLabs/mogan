<TeXmacs|2.1.2>

<style|<tuple|tmdoc|devel>>

<\body>
  <tmdoc-title|Avoid retrieving large pictures from texmacs website>

  When using some theme, <TeXmacs> will download large image files(~2MiB)
  from <slink|http://texmacs.org/artwork/>, which is extremely slow for users
  in China. So we should remove this feature, and bundle relative image files
  into installer.

  <section|Tasks>

  <\wide-tabular>
    <tformat|<table|<row|<\cell>
      Chore
    </cell>|<\cell>
      6_1
    </cell>|<\cell>
      jingkaimori
    </cell>|<\cell>
      Replace thumbnails with origin image.
    </cell>>|<row|<\cell>
      Chore
    </cell>|<\cell>
      6_2
    </cell>|<\cell>
      jingkaimori
    </cell>|<\cell>
      Adjust links like <slink|tmfs://artworks/pictures/blackboard/blackboard-pxhere.jpg>
    </cell>>|<row|<\cell>
      Chore
    </cell>|<\cell>
      6_3
    </cell>|<\cell>
      jingkaimori
    </cell>|<\cell>
      Remove definition of <slink|tmfs://artworks/>
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
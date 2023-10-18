<TeXmacs|2.1.2>

<style|<tuple|tmdoc|chinese|devel>>

<\body>
  <tmdoc-title|Merge contiguous indices for sorted bib items>

  <section|Feature metadata>

  <\description>
    <item*|Owner><value|jk>

    <item*|Reporter>Zhengfei Hu

    <item*|Savannah patch><hlink|10339|https://savannah.gnu.org/patch/?10339>

    <item*|Github issue><hlink|166|https://github.com/XmacsLabs/mogan/issues/166>
  </description>

  <section|Description>

  Merge continuous citing indices. Users should add <tmpackage|cite-sort>
  package to enable this feature.

  <tabular|<tformat|<twith|table-hmode|min>|<twith|table-width|1par>|<cwith|1|-1|1|-1|cell-hyphen|t>|<cwith|1|1|1|-1|cell-tborder|0ln>|<cwith|1|1|1|-1|cell-bborder|1ln>|<cwith|2|2|1|-1|cell-tborder|1ln>|<cwith|1|1|1|1|cell-lborder|0ln>|<cwith|1|1|2|2|cell-rborder|0ln>|<table|<row|<\cell>
    Origin
  </cell>|<\cell>
    Merged
  </cell>>|<row|<\cell>
    [1, 2, 3]
  </cell>|<\cell>
    [1-3]
  </cell>>|<row|<\cell>
    [1, 2, 3, 4, 5]
  </cell>|<\cell>
    [1-5]
  </cell>>|<row|<\cell>
    [1, 3, 4, 5, 7]
  </cell>|<\cell>
    [1, 3-5, 7]
  </cell>>|<row|<\cell>
    [1, 2, 4, 5]
  </cell>|<\cell>
    [1, 2, 4, 5]
  </cell>>>>>

  Origin patch is provided by Zhengfei Hu(hammer).

  <tmdoc-copyright|2023|jingkaimori>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>.
</body>

<\initial>
  <\collection>
    <associate|page-medium|paper>
  </collection>
</initial>